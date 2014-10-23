
use Test::More qw/no_plan/;
use File::Temp qw/tempfile/;
#use YAML;

# TODO:
#  check type of token recognised

use strict;
use warnings;

package LineGiver;
# simple class which has a getline() method which returns the next line

sub new {
    my $class = shift;
    my $self = [];
    bless $self, $class;
}

sub addtokens {
    my $self = shift;
    for my $token (@_) {
        if (@$self==0 || $self->[-1] =~ /[\015\012]$/) {
            unshift @$self, $token;
        } else {
            $self->[-1] .= $token;
        }
    }
}

sub get_next_line {
    my $self = shift;
    pop @$self;
}

package main;

BEGIN {
    use_ok('Hardware::Vhdl::Lexer');
}

if (1) {
    # history tests
    for my $nhist (4, 6) {
        diag("history tests with nhistory=$nhist");
        my $fh = &string_to_file("foo bar baz");
        my $tp = Hardware::Vhdl::Lexer->new(linesource => $fh, nhistory => $nhist);
        isa_ok( $tp, 'Hardware::Vhdl::Lexer', "new Hardware::Vhdl::Lexer" );
        my $i;
        for $i (0..$nhist-1) {
            is($tp->history($i), '', "history test 0 item $i")
        }
        for $i ($nhist..7) {
            eval { $tp->history($i) };
            my $e = $@;
            my $n = $i + 1;
            like($e, qr/^more \($n\) history requested than has been stored \($nhist\)/,  "history test 0 item $i")
        }
        
        is(scalar $tp->get_next_token, 'foo', 'get_next_token 1');
        is($tp->history(0), 'foo', "history test 1 item 0");
        for $i (1..$nhist-1) {
            is($tp->history($i), '', "history test 1 item $i")
        }
        for $i ($nhist..7) {
            eval { $tp->history($i) };
            my $e = $@;
            my $n = $i + 1;
            like($e, qr/^more \($n\) history requested than has been stored \($nhist\)/,  "history test 1 item $i")
        }
        
        is(scalar $tp->get_next_token, ' ', 'get_next_token 2');
        is($tp->history(0), 'foo', "history test 2 item 0");
        is($tp->history(1), '', "history test 2 item 0");
        is(scalar $tp->get_next_token, 'bar', 'get_next_token 3');
        is($tp->history(0), 'bar', "history test 3 item 0");
        is($tp->history(1), 'foo', "history test 3 item 1");
        for $i (2..$nhist-1) {
            is($tp->history($i), '', "history test 1 item $i")
        }
    }
}

if (1) {
    diag("token splitting tests:");
    &check_splitting(['report','"delay time between RD# and RD# violated"', ';'], "string 1", 'fileglob');
    &check_splitting(['report','"delay time between RD# and RD# violated"', ' '], "string 2", 'fileglob');
    &check_splitting(['report','"RD\\\\"', ';'], "string 3", 'class');
    &check_splitting(['report','"RD\\\\\\\\"', ' '], "string 4", 'class');
    &check_splitting(['abc',' ','<=','  ','afunc','(','t',',',"'0'",')',';',' ','-- a comment',"\015\012",
        'def',':=','"string with \\" quotes in"'], "token splitting 1", 'class');
    &check_splitting(['def','<=','\\$%^&*()\\','&','\\_dFe{}\\'], "extended identifiers", 'class');
    &check_splitting(['def','<=','"string with \015\012 newline in"'], "string with newline in", 'class');
    &check_splitting(['def','<=','"string with \015\012 several \015\012 newlines in"'], "string with newlines in", 'class');
    &check_splitting(['def','<=','"unterminated string '], "unterminated string", 'class');
    &check_splitting(['def','<=','"unterminated \\" string '], "unterminated string 2", 'class');
    &check_splitting(['abc',"\015\012",'abc',"\015\012","\015\012",'abc',"\015",'def',"\012",'ghi',"\012\015",'jkl'], "newlines", 'class');
    &check_splitting([
        'abc',':=','1_000.31',';',
        'abc',':=','1_000.E6',';',
        'abc',':=','-1_000.31E-6',';',
        'abc',':=','2#11_0101#',';',
        'abc',':=','8#12_3457#',';',
        'abc',':=','16#AB_5AFD#',';',
        ], "numeric literals", 'class');
    &check_splitting([
        'abc',':=','X"AA55"',';',
        'abc',':=','O"353"',';',
        'abc',':=','B"11_0101"',';',
        ], "bit_vector literals", 'class');
    &check_splitting([ ['GENERIC', 'ci'], ['(', 'cp'], [' ', 'ws'], ["\015\012", 'wn'], ['    ', 'ws'], ['ADR_WID', 'ci'] ], "whitespace then newline", 'class');
}

ok( 1, 'End of tests' );

sub string_to_file {
    my $string = shift;
    my $fh = tempfile;
    binmode $fh;
    print $fh $string;
    seek $fh, 0, 0;
    $fh;
}

sub check_splitting {
    my ($tokens, $testname, $sourcetype) = @_;
    my @correct_tokens = @$tokens;
    my $tp;
    
    if ($sourcetype eq 'class') {
        my $linegiver = LineGiver->new;
        for my $ti (@correct_tokens) {
            $linegiver->addtokens(ref $ti eq 'ARRAY' ? $ti->[0] : $ti);
        }
        $tp = Hardware::Vhdl::Lexer->new(linesource => $linegiver);
    } else {
        my $source;
        for my $ti (@correct_tokens) {
            $source .= ref $ti eq 'ARRAY' ? $ti->[0] : $ti;
        }
        my $fh = &string_to_file($source);
        $tp = Hardware::Vhdl::Lexer->new(linesource => $fh);
    }
    
    push @correct_tokens, undef;
    my @got_tokens;
    while (@got_tokens < @correct_tokens) {
        if (ref $correct_tokens[scalar @got_tokens] eq 'ARRAY') {
            push @got_tokens, [$tp->get_next_token];
        } else {
            push @got_tokens, scalar $tp->get_next_token;
        }
    }
    #if (ref $got_tokens[0] eq '' && $got_tokens[0] ne $correct_tokens[0]) {
    #    print "expecting:".Dump(\@correct_tokens);
    #    print "got:".Dump(\@got_tokens);
    #}
    is_deeply(\@got_tokens, \@correct_tokens, $testname);
}