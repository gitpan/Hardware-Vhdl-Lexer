package Hardware::Vhdl::Lexer;

=head1 NAME

Hardware::Vhdl::Lexer - Split VHDL code into lexical tokens

=head1 SYNOPSIS

    use Hardware::Vhdl::Lexer;
    use Hardware::Vhdl::PreProcess;
    my $fh;
    open $fh, '<', 'device_behav.vhd' || die $!
    my $lexer = Hardware::Vhdl::Lexer->new(linesource => $fh);
    my ($token, $type);
    while( (($token, $type) = $lexer->get_next_token) && defined $token) {
        print "# type = '$type' token='$token'\n";
    }

    # use a class to supply the source code lines, and ask the lexer to remember the last 5 code tokens.
    my $lexer = Hardware::Vhdl::Lexer->new(
        linesource => Hardware::Vhdl::PreProcess->new(sourcefile => 'device_behav.vhd'),
        nhistory => 5
        );

=head1 DESCRIPTION

C<Hardware::Vhdl::Lexer> splits VHDL code into lexical tokens.  To use it, you need to first create a lexer object, passing in an
object with a C<get_next_line> method which will supply lines of VHDL code to the lexer.  Repeated calls to the C<get_next_token> method of
the lexer will then return VHDL tokens (in scalar context) or a token type code and the token (in list context).  C<get_next_token>
returns undef when there are no more tokens to be read.

=cut

# to do:
#  accept filehandle references for linesource

use Carp;
use strict;
use warnings;

our $VERSION = "0.04";

=head1 CONSTRUCTOR

=over 4

=item new(linesource => $object_with_get_next_line_method [, nhistory => N])

=item new(linesource => $filehandle_reference [, nhistory => N])

=item new(linesource => \@array_of_lines [, nhistory => N])

=item new(linesource => \$scalar_containing_vhdl [, nhistory => N])

=item new(linesource => \&subroutine_that_returns_lines [, nhistory => N])

The linesource argument is required: it can be either an object with a C<get_next_line> method, or a filehandle reference, in which case C<readline> will
be used on it.  The linesource is expected to return undef when there are no more lines to read.  The optional nhistory argument sets 
how many "code" tokens (see below) will be remembered for access by the C<history> method.

=back
=cut

sub new {
    my $class = shift;
    my %args = @_;
    my $self = {
        nhistory => 1,
        line => '',
        source_is_fileglob => 0,
        linesource => undef,
    };
    
    # copy args to self
    for my $argname (qw/ nhistory linesource /) {
        if (exists $args{$argname}) {
            $self->{$argname} = $args{$argname};
            delete $args{$argname};
        }
    }
    
    #! TODO: check there are no passed args left
    
    # check that a linesource was specified
    croak "$class constructor requires a linesource to be specified" unless defined $self->{linesource};
    
    {
        my $sourcetype = ref $self->{linesource};
        if ($sourcetype eq 'GLOB') { $self->{source_func} = sub { readline($self->{linesource}) } }
        elsif ($sourcetype eq 'ARRAY') { my $i=0; $self->{source_func} = sub { $self->{linesource}[$i++] } }
        elsif ($sourcetype eq 'SCALAR') { my $i=0; $self->{source_func} = sub { $i++ == 0 ? $self->{linesource} : undef } }
        elsif ($sourcetype eq 'SUB') { $self->{source_func} = $self->{linesource} }
        elsif ($sourcetype && eval { "$sourcetype->can('get_net_line')" } ) { $self->{source_func} = sub { $self->{linesource}->get_next_line } }
        else { croak "${class}->new 'linesource' parameter is not of a valid type" }
    }
    
    # set up initial history values
    for my $i (1..$self->{nhistory}) { $self->{history}[$i-1] = '' }
    
    bless $self, $class;
}

=head1 METHODS

=over 4

=item linesource()

Returns the linesource argument passed into the constructor

=cut

sub linesource { $_[0]->{linesource} }

# sourcefile, linenum and files_used methods are deprecated: call methods of $lexer->linesource instead
#sub sourcefile { $_[0]->{linesource}->sourcefile }
#sub linenum { $_[0]->{linesource}->linenum }
#sub files_used { $_[0]->{linesource}->files_used }

=item C<get_next_token()>

In scalar context, returns the next VHDL token.

In list context, returns a token type code and the token

Nothing is removed from the source code: if you concatenate all the tokens returned by C<get_next_token()>, you will get the same
result as if you concatenate all the strings returned by the linesource object.

The token type codes are 1 or 2-character strings.  When the codes are 2 characters, the first character gives the general class of the
token and the second indicates its type more specifically.  The first character will be 'w' for whitespace, 'r' for comments (remarks) 
or 'c' for code.  It should be possible to remove all comment tokens, and change whitespace tokens for different whitespace, and 
always end up with functionally equivalent code.

The token type codes are:

=over 4

=item wn

Whitespace:Newline.  This could be any of \012, \015, \015\012 or \012\015.

=item ws

Whitespace:Spaces.  A group of whitespace characters which match the /s regexp pattern but which do not include any carriage-return
or linefeed characters.

=item r

Remark.  The token will start with two dashes and include the remainder of the source code line, not including any newline characters.  
The next token will either be a newline or undef.

=item cs

Code:String literal.  The lexer accepts multi-line strings, even though the VHDL specification does not allow them.

=item cc

Code:Character literal.

=item cb

Code:Bit_vector literal.  For example, C<B"001_1010"> or C<O"7720"> or C<H"A7_DEAD">.

=item cn

Code:Numeric literal.  This could be a specified-base literal like C<8#7720#> or a simple integer or floating-point value.

=item ci

Code:Identifier or keyword.  For example, C<package> or C<my_signal_23> or C</extended identifier$%!/>..

=item cp

Code:Punctuation.  A group of punctuation symbols which cannot be part of an extended identifier, and are not separated by whitespace.

=back 

=cut

sub get_next_token {
    my $self = shift;
    my $token;
    my $match = '--';
    if (defined $self->{line} && $self->{line} eq '') {
        $self->{line} = &{$self->{source_func}}
    }
    return undef unless defined $self->{line};
    
    if ($self->{line} =~ m/^(\015\012?|\012\015?)(.*)$/s) { # newline
        $token = $1;
        $self->{line} = $2;
        $match = 'wn';
    } elsif ($self->{line} =~ m/^([^\S\012\015]+)(.*)$/s) { # whitespace
        $token = $1;
        $self->{line} = $2;
        $match = 'ws';
    } elsif (substr($self->{line},0,1) eq '"') { # string literal
        # keep reading lines from the source file until we find a non-escaped closing quote or EOF
        FIND_END_QUOTE: { 
            if ( $self->{line} =~ /^(" .*? (?<!\\) (?:\\\\)* ") (.*) $/xs ) {
                $token = $1;
                $self->{line} = $2;
            } else {
                my $t = $self->{line};
                $self->{line} = &{$self->{source_func}};
                if (!defined $self->{line}) {
                    # reached EOF without finding closing quote: we're done
                    $token = $t;
                } else {
                    $self->{line} = $t.$self->{line};
                    redo FIND_END_QUOTE;
                }
            }
        }
        $match = 'cs';
    } elsif ($self->{line} =~ m/^--/) { # comment
        if ($self->{line} =~ m/(.*?)((\015\012?|\012\015?).*)/s) {
            $token = $1;
            $self->{line} = $2;
        } else {
            $token = $self->{line};
            $self->{line} = '';
        }
        $match = 'r';
    } elsif ($self->{line} =~ m/^('.')(.*)$/s) { # single-character literal
        $token = $1;
        $self->{line} = $2;
        $match = 'cc';
    } elsif ($self->{line} =~ m/^([BOX]".+?")(.*)$/s) { # bit_vector literal
        $token = $1;
        $self->{line} = $2;
        $match = 'cb';
    } elsif ($self->{line} =~ m/^((2|3|4|5|6|7|8|9|10|11|12|13|14|15|16)#[\d_A-F]+#)(.*)$/si) { # specified-base integer numeric literal
        $token = $1;
        $self->{line} = $3;
        $match = 'cn';
    } elsif ($self->{line} =~ m/^(-?\d[\d_]*(\.\d*)?(E-?\d+)?)(.*)$/si) { # base-10 numeric literal
        $token = $1;
        $self->{line} = $4;
        $match = 'cn';
    } elsif ($self->{line} =~ m/^(\\.+?\\)(.*)$/s) { # extended identifier
        $token = $1;
        $self->{line} = $2;
        $match = 'ci';
    } elsif ($self->{line} =~ m/^(\w+)(.*)$/s) { # keyword or identifier
        $token = $1;
        $self->{line} = $2;
        $match = 'ci';
    } elsif ($self->{line} =~ m/^(:=|<=|=>|[^\w\s])(.*)$/s) { # punctuation
        $token = $1;
        $self->{line} = $2;
        $match = 'cp';
    }
    if (substr($match, 0, 1) eq 'c') {
        # not whitespace or comment, so add it to the code history
        push @{$self->{history}}, $token;
        while (@{$self->{history}} > $self->{nhistory}) { shift @{$self->{history}} };
    }
    #print "# token=".(defined $token ? "'$token'" : 'undef')." type = '$match'\n";
    wantarray ? ($token, $match) : $token;
}

=item history(N)

Returns previous code tokens.  N must not be larger than the nhistory argument passed to the constructor.  C<history(0)> will 
return the text of the last token returned by C<get_next_token> whose type started with a 'c',
C<history(1)> will return the code token before that, and so on.

=cut

sub history {
    my $self = shift;
    my $age = shift;
    #$age < @{$self->{history}} ? $self->{history}[-1 - $age] : undef;
    croak "more (".($age+1),") history requested than has been stored (".($self->{nhistory}).")" if $age >= @{$self->{history}};
    $self->{history}[-1 - $age];
}


=back

=head1 AUTHOR

Michael Attenborough, C<< <michael.attenborough at physics.org> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-hardware-vhdl-lexer at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Hardware-Vhdl-Lexer>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Hardware::Vhdl::Lexer

You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Hardware-Vhdl-Lexer>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Hardware-Vhdl-Lexer>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Hardware-Vhdl-Lexer>

=item * Search CPAN

L<http://search.cpan.org/dist/Hardware-Vhdl-Lexer>

=back

=head1 ACKNOWLEDGEMENTS

=head1 COPYRIGHT & LICENSE

Copyright 2006 Michael Attenborough, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1; # End of Hardware::Vhdl::Lexer
