# vi: set autoindent shiftwidth=4 tabstop=8 softtabstop=4 expandtab:
package DB;

use 5.006001;
use strict;
use warnings;

use Carp;
use B qw(svref_2object comppadlist class);
use B::Showlex;
use Curses;
use Curses::UI;
use Data::Dumper;

use Devel::PDB::Source;

use vars qw(*dbline);

our $VERSION = '0.01';

our $single;
our $sub;
our $trace;
our $signal;
our $stack_depth;
our @stack;
our $current_sub;

my @complied;
my $inited = 0;
my $cui;
my $sv_win;
my $sv;
my $exit = 0;
my $yield;
my %sources;
my $new_single;
my $current_source;
my $evalarg;
my $package;
my $filename;
my $line;
my @watch_exprs;

my $auto_win;
my $watch_win;
my $padvar_list;
my $watch_list;

my $padlist_scope;
my %padlist;
my @padlist_disp;

my $stdout;
my $output;

$trace = $signal = $single = 0;
$stack_depth = 0;
@stack = (0);

END {
    open STDOUT, ">&", $stdout;
    $single = 0;
}

our %def_style = (
    -bg => 'white',
    -fg => 'blue',
    -bbg => 'blue',
    -bfg => 'white',
    -tbg => 'white',
    -tfg => 'blue',
);

sub db_quit {
    return if not $cui->dialog(
        -title => 'Quit Debugger',
        -buttons => ['yes', 'no'],
        -message => 'Do you really want to quit?',
        %def_style,
    );
    $single = 0;
    for (my $i = 0; $i <= $stack_depth; ++$i) {
        $stack[$i] = 0;
    }
    #print(STDERR $_, "\n") foreach (@complied);
    exit(0);
}

sub db_cont {
    $new_single = 0;
    for (my $i = 0; $i <= $stack_depth; ++$i) {
        $stack[$i] &= ~1;
    }
    $yield = 1;
}

sub db_step_in {
    $new_single = 1;
    $yield = 1;
}

sub db_step_over {
    $new_single = 2;
    $yield = 1;
}

sub db_step_out {
    $new_single = 0;
    $stack[-1] &= ~1;
    $yield = 1;
}

sub db_toggle_break {
    local (*dbline) = $main::{'_<' . $current_source->filename};
    $current_source->toggle_break;
}

sub db_add_watch_expr {
    my $expr = $cui->question(-question => 'Please enter an expression to watch for', %def_style);
    return if !$expr;
    push @watch_exprs, { name => $expr };
    &ui_update_watch_list;
    $cui->draw;
}

sub ui_open_file {
    my ($title, $files) = @_;

    my $filename = $cui->tempdialog('Devel::PDB::Dialog::FileBrowser',
        -title => $title,
        -files => $files,
	%def_style,
    );
    if ($filename) {
        my $source = $current_source = get_source($filename);
        $sv->source($source) if $source;
        $sv->intellidraw;
    }
}

sub ui_adjust_vert_parts {
    my $delta = shift;
    return if $delta > 0 && $sv_win->{-padbottom} >= $cui->{-height} - $sv_win->{-padtop} - 5 or
        $delta < 0 && $auto_win->{-height} <= 5;
    $sv_win->{-padbottom} += $delta;
    $auto_win->{-height} += $delta;
    $watch_win->{-height} += $delta;
    $cui->layout_contained_objects;
}

sub ui_adjust_hori_parts {
    my $delta = shift;
    return if $delta > 0 && $auto_win->{-width} >= $cui->{-width} - 15 or
        $delta < 0 && $auto_win->{-width} <= 15;
    $auto_win->{-width} += $delta;
    $watch_win->{-padleft} += $delta;
    $cui->layout_contained_objects;
}

sub init {
    # can anybody tell me why $win->notimeout(1) doesn't work?
    $ENV{ESCDELAY} = '0';

    $cui = new Curses::UI( 
        -clear_on_exit => 1,
        -color_support => 1
    );

    if ($Curses::UI::color_support) {
        my $old_draw = \&Curses::UI::Widget::draw;
        no warnings;
        *Curses::UI::Widget::draw = sub (;$) {
            my ($this) = @_;
            if (defined $this->{-fg} && defined $this->{-bg}) {
                my $canvas = defined $this->{-borderscr} ? $this->{-borderscr} : $this->{-canvasscr};
                $canvas->bkgdset(COLOR_PAIR($Curses::UI::color_object->get_color_pair($this->{-fg}, $this->{-bg})));
            }
            &$old_draw(@_);
        };
    }

    my $lower_height = int($cui->{-height} * 0.25);
    my $half_width = int($cui->{-width} * 0.5);

    $sv_win = $cui->add(
        'sv_win', 'Window',
        -padtop => 1,
        -padbottom => $lower_height,
        -border => 0,
        -ipad => 0,
        -title => 'Source',
    );
    $sv = $sv_win->add(
        'sv', 'Devel::PDB::SourceView',
        -border => 1,
        #-padbottom => 3,
        %def_style,
    );
    
    $auto_win = $cui->add(
        'auto_win', 'Window',
        -border => 1,
        -y => -1,
        -width => $half_width,
        -height => $lower_height,
        -title => 'Auto',
        %def_style,
    );
    $padvar_list = $auto_win->add(
        'padvar_list', 'Devel::PDB::NamedListbox',
        -readonly => 1,
        -named_list => \@padlist_disp,
    );

    $watch_win = $cui->add(
        'watch_win', 'Window',
        -border => 1,
        -x => -1,
        -y => -1,
        -padleft => $half_width,
        -height => $lower_height,
        -title => 'Watch',
        %def_style,
    );
    $watch_list = $watch_win->add(
        'watch_list', 'Devel::PDB::NamedListbox',
        -named_list => \@watch_exprs,
    );

    $cui->add(
        'menu', 'Menubar',
        -menu => [
            { -label => 'File', -submenu => [
                { -label => 'Exit', -value => \&db_quit },
            ] },
            { -label => 'Help', submenu => [
                { -label => 'About', -value => sub { } },
            ] },
        ],
        %def_style,
    );

    $cui->set_binding(\&db_quit, "\cQ", "\cC");
    $cui->set_binding(\&db_cont, KEY_F(5));
    $cui->set_binding(\&db_step_out, KEY_F(6));
    $cui->set_binding(\&db_step_in, KEY_F(7));
    $cui->set_binding(\&db_step_over, KEY_F(8));
    $cui->set_binding(\&db_toggle_break, KEY_F(9));
    $cui->set_binding(sub { ui_open_file('Compiled Files', \@complied); }, KEY_F(11));
    $cui->set_binding(sub { ui_open_file('Opened Files', [keys(%sources)]); }, KEY_F(12));
    $cui->set_binding(sub { shift->getobj('menu')->focus }, KEY_F(10));

    $cui->set_binding(\&db_add_watch_expr, "\cW");

    $cui->set_binding(sub { $sv_win->focus }, KEY_F(1));
    $cui->set_binding(sub { $auto_win->focus }, KEY_F(2));
    $cui->set_binding(sub { $watch_win->focus }, KEY_F(3));
    
    $cui->set_binding(sub { ui_adjust_vert_parts(1) }, '{');	
    $cui->set_binding(sub { ui_adjust_vert_parts(-1) }, '}');	
    $cui->set_binding(sub { ui_adjust_hori_parts(-1) }, '[');	
    $cui->set_binding(sub { ui_adjust_hori_parts(1) }, ']');	

    #open my $fd0, '>stdout';
    #open my $fd1, '>stderr';
    #open STDOUT, ">&$fd0";
    #open STDERR, ">&$fd1";
    #open STDOUT, ">stdout";

    open STDERR, ">stderr";
    open $output, ">stdout";
    open $stdout, ">&STDOUT";

    $inited = 1;
}

sub get_source {
    my $filename = shift;
    my $source = $sources{$filename};

    if (!defined $source) {
        local (*dbline) = $main::{"_<$filename"};
        $sources{$filename} = $source = new Devel::PDB::Source(
            filename => $filename,
            lines => \@dbline,
            breaks => \%dbline,
        );
    }

    return $source;
}

my @saved;

sub save {
    @saved = ($@, $!, $,, $/, $\, $^W);
    $, = '';
    $/ = "\n";
    $\ = '';
    $^W = 0;
}

sub eval {
    ($@, $!, $,, $/, $\, $^W) = @saved;
    my $res = eval "package $package; $evalarg";
    save;
    $res;
}

sub ui_update_watch_list {
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent;

    foreach my $expr (@watch_exprs) {
        $evalarg = $expr->{name};
        my $res = &DB::eval;
        $Data::Dumper::Indent = 0;
        $expr->{value} = Dumper $res;
        $Data::Dumper::Indent = 1;
        $expr->{long_value} = Dumper $res;
    }

    $watch_list->update;
}

sub DB  {
    return if $exit;
    save;
    init if !$inited;

    open STDOUT, ">&", $stdout;

    ($package, $filename, $line) = caller;

    my $scope = $current_sub ? $current_sub : $package;
    my $renew = !defined $padlist_scope || $scope ne $padlist_scope;
    if ($renew) {
        %padlist = ();
        @padlist_disp = ();
        $padlist_scope = $scope;
    }

    my ($names, $vals) = $scope eq 'main' ? comppadlist->ARRAY : svref_2object(\&$scope)->PADLIST->ARRAY;
    my @names = $names->ARRAY;
    my @vals  = $vals->ARRAY;
    my $count = @names;

    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent;
    for (my ($i, $j) = (0, 0); $i < $count; $i++) {
        my $sv = $names[$i];
        next if class($sv) eq 'SPECIAL';
        my $name = $sv->PVX;
        $Data::Dumper::Indent = 0;
        my $val = Dumper $vals[$i]->object_2svref;
        $val =~ s/^\\// if class($sv) ne 'RV';
        $Data::Dumper::Indent = 1;
        my $long_val = Dumper $vals[$i]->object_2svref;
        $long_val =~ s/^\\// if class($sv) ne 'RV';
        if ($renew || $val ne $padlist{$name}) {
            $padlist_disp[$j] = { name => $name, value => $val, long_value => $long_val };
            $padlist{$name} = $val;
        }
        ++$j;
    }
    $padvar_list->update($renew);

    #local (*dbline) = $main::{"_<$filename"};
    $sv->source($current_source = get_source($filename));
    $current_source->current_line($line);

    ui_update_watch_list;

    $yield = 0;
    $new_single = $single;
    $sv->focus;
    $cui->focus(undef, 1);
    $cui->draw;
    $cui->do_one_event while !$yield;
    $single = $new_single;

    open STDOUT, ">&", $output;
    ($@, $!, $,, $/, $\, $^W) = @saved;
}

sub sub {
    my ($ret, @ret);

    local $current_sub = $sub;
    local $stack_depth += 1;
    $#stack = $stack_depth;
    $stack[-1] = $single;
    $single &= 1;

    if (wantarray) {
        no strict;
        @ret = &$sub;
        use strict;
        $single |= $stack[$stack_depth--];
        @ret;
    } else {
        if (defined wantarray) {
            no strict;
            $ret = &$sub;
            use strict;
        } else {
            no strict;
            &$sub;
            use strict;
            undef $ret;
        }

        $single |= $stack[$stack_depth--];
        $ret;
    }
}

sub postponed {
    my $file = shift;
    push @complied, $$file;
}

package Devel::PDB;

1;

__END__

=head1 NAME

Devel::PDB - A simple Curses-based Perl Debugger

=head1 SYNOPSIS

    perl -d:PDB foo.pl

=head1 DESCRIPTION

PerlDeBugger is a Curses-based Perl debugger with most of the
essential functions such as monitoring windows for paddlist,
call stack, custom watch expressions, etc. Suitable for debugging or
tracing complicated Perl applications on the spot.

=begin SCREENSHOT

<style>
.normal {
    font-family: Courier New;
    font-size: 10pt;
    font-weight: bold;
    background-color: #B2B2B2;
    color: #4545B2
}
.border {
    background-color: #4545B2;
    color: #B2B2B2;
}
.caption {
    color: #FFFFFF;
}
</style>
<span class="normal">
&nbsp;&nbsp;File&nbsp;&nbsp;Help&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br>
<span class="border">┌<span class="caption">&nbsp;a.pl:5&nbsp;</span>──────────────────────────────────────────────────────────────────────┐</span><br>
<span class="border">|</span>&nbsp;&nbsp;use&nbsp;Devel::PDB;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">|</span>&nbsp;&nbsp;#!/usr/bin/perl&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">|</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">|</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">|</span><span class="border">&nbsp;&nbsp;<span class="normal">m</span>y&nbsp;$a&nbsp;=&nbsp;test();&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</span>&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">|</span>&nbsp;&nbsp;print&nbsp;"$a\n";&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">|</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">|</span>&nbsp;&nbsp;sub&nbsp;test&nbsp;{&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">|</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;my&nbsp;$hey&nbsp;=&nbsp;10;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">|</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;my&nbsp;$guys_this_is_long&nbsp;=&nbsp;20;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">|</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;test2();&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">|</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;2;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">|</span>&nbsp;&nbsp;}&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">|</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">|</span>&nbsp;&nbsp;sub&nbsp;test2&nbsp;{&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">└──────────────────────────────────────────────────────────────────────────────┘</span><br>
<span class="border">┌<span class="caption">&nbsp;Auto&nbsp;</span>────────────────────────────────┐┌<span class="caption">&nbsp;Watch&nbsp;</span>───────────────────────────────┐</span><br>
<span class="border">|</span>$a&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;undef&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><span class="border">|</span>-&nbsp;no&nbsp;values&nbsp;-&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">|</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><span class="border">|</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">|</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><span class="border">|</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">|</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><span class="border">|</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="border">|</span><br>
<span class="border">└──────────────────────────────────────┘└──────────────────────────────────────┘</span><br>
</span>

=end SCREENSHOT

=head2 FUNCTIONS

PerlDeBugger is still in development stage, not all of the planed
functions have been implemented yet. Currently it can:

=over

=item *

step-over, step-in, step-out, run

=item *

set/remove breakpoint

=item *

automatic display of lexical variables

=item *

add/remove custom watch expression

=item *

show/open compiled files

=back

TODO (sorted by priority):

=over

=item *

Stack Trace Window

=item *

STDOUT/STDERR Window (currently STDOUT and STDERR are redirected to files B<stdout> and B<stderr> in the current directory)

=item *

Immediate Window for executing arbitrary perl statement

=item *

Load/Save debugger workspace (breakpoints, watch expressions, etc.)

=back

=head2 KEYS

=over

=item Global

=over

=item F1

Switch to the Source Code Window

=item F2

Switch to the Lexical Variable Window

=item F3

Switch to the Watch Window

=item F5

Continue execution

=item F6

Step Out

=item F7

Step In

=item F8

Step Over

=item F9

Toggle Breakpoint

=item F10

Show 'I<Compiled Files>' Dialog

=item F11

Show 'I<Opened Files>' Dialog

=item Ctrl+Q

Quit the debugger

=item Ctrl+W

Add watch expression

=back

=item Source Code Window

=over

=item UP/DOWN/LEFT/RIGHT/PAGE UP/PAGE DOWN

Move the cursor

=item H/J/K/L/Ctrl+F/Ctrl+B

If you use VI, you will know

=item /

Search using a RegEx in the current opened file

=item n

Search Next

=item N

Search Previous

=item Ctrl+G

Goto a specific line

=back

=item Lexical Variable Window / Watch Window

=over

=item UP/DOWN

Move the cursor

=item ENTER

Show the Data::Dumper output of the highlighted item in a scrollable dialog

=item DEL

Remove the highlighted expression (Watch Window only)

=back

=item Compiled File Dialog / Opened File Dialog

=over

=item TAB

Toggle the focus between the file list and the filter

=item ENTER

Select the highlighted file or apply the filter to the file list

=back

=back

=head1 SEE ALSO

L<perldebug>

=head1 AUTHOR

Ivan Yat-Cheung Wong E<lt>email@ivanwong.infoE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2007 by Ivan Y.C. Wong 黃溢章

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.8 or,
at your option, any later version of Perl 5 you may have available.

=cut
