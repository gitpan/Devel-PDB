# vi: set autoindent shiftwidth=4 tabstop=8 softtabstop=4 expandtab:
package Devel::PDB::Source;
use strict;
use warnings;

use base qw(Class::Accessor::Faster);

use Carp;
use Data::Dumper;

__PACKAGE__->mk_accessors(qw(
    filename
    lines
    breaks
    current_line
    scr_x
    scr_y
    cur_x
    cur_y
    view
));

sub new {
    my $class = shift;
    my %params = @_;
    
    my $this = $class->SUPER::new({
        filename => undef,
        lines => undef,
        breaks => undef,
        current_line => -1,
        scr_x => 0,
        scr_y => 0,
        cur_x => 0,
        cur_y => 0,
        @_,
    });

    croak 'undefined source?' unless defined $this->lines;
    croak 'undefined breakpoint table?' unless defined $this->breaks;

    return $this;
}

sub current_line {
    my $this = shift;
    my $ret = $this->_current_line_accessor(@_);

    if (@_) {
        $this->cur_x(0);
        $this->cur_y(@_);

        my $view = $this->view;
        $view->scroll_to_line if @_ && defined $view;
    }
    
    return $ret;
}

sub cur_y {
    my $this = shift;

    if (@_) {
        my ($line) = @_;
        my $line_cnt = scalar @{$this->lines};

        $line = 0 if $line < 0;
        $line = $line_cnt - 1 if $line >= $line_cnt;

        my $ret = $this->_cur_y_accessor($line);

        return $ret;
    }
    
    return $this->_cur_y_accessor;
}

sub toggle_break {
    my $this = shift;
    my $breaks = $this->breaks;
    my $line = $this->cur_y;
    my $line_cnt = scalar @{$this->lines};
    my $view = $this->view;
    my $ret = 0;

    ++$line while (${$this->lines}[$line] == 0 && $line_cnt > $line);
    return 0 if $line_cnt <= $line;

    if ($breaks->{$line}) {
	$breaks->{$line} = 0;
	delete $breaks->{$line};
    } else {
	$ret = $breaks->{$line} = 1;
    }

    if (defined $view) {
        if ($ret && $this->cur_y != $line) {
            $view->cursor_down(undef, $line - $this->cur_y);
        } else {
            $view->intellidraw;
        }
    }
    
    return $ret;
}

1;
