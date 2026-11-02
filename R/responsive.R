# ---------------------------------------------------------------------------
# Responsive windows: every window (main window, dialogs) adapts to the
# screen size so that all its elements, including the bottom buttons, stay
# visible without scrolling.
#  - the requested size is capped to the screen (taskbar and title bar kept);
#  - a window whose content is larger than its geometry is enlarged up to the
#    screen limit;
#  - if the content still does not fit, the Tk default fonts are reduced
#    step by step (and restored when the window is closed);
#  - a window that goes off-screen is moved back.
# ---------------------------------------------------------------------------

.rsp_tcl_script = '
namespace eval ::rsp {
  variable base
  variable shrunk
  variable flag
  array set base {}
  array set shrunk {}
  array set flag {}
}
proc ::rsp::shrink {w} {
  variable base
  variable shrunk
  set changed 0
  foreach f {TkDefaultFont TkTextFont TkFixedFont TkMenuFont TkHeadingFont} {
    if {[catch {font configure $f -size} s]} continue
    if {![info exists base($f)]} { set base($f) $s }
    if {$s < 0} {
      set ns [expr {int($s * 0.9)}]
      if {$ns > -9} { set ns -9 }
    } else {
      set ns [expr {$s - 1}]
      if {$ns < 7} { set ns 7 }
    }
    if {$ns != $s} {
      catch {font configure $f -size $ns}
      set changed 1
    }
  }
  if {$changed && ![info exists shrunk($w)]} {
    set shrunk($w) 1
    bindtags $w [linsert [bindtags $w] 0 RspTag]
  }
  return $changed
}
proc ::rsp::gone {w} {
  variable base
  variable shrunk
  unset -nocomplain shrunk($w)
  if {[array size shrunk] == 0} {
    foreach {f s} [array get base] { catch {font configure $f -size $s} }
    array unset base
  }
}
proc ::rsp::fit {w} {
  variable flag
  if {![winfo exists $w]} return
  if {[winfo toplevel $w] ne $w} return
  if {[winfo class $w] ne {Toplevel}} return
  if {[catch {wm overrideredirect $w} ov] == 0 && $ov} return
  if {[wm state $w] ne {normal}} return
  update idletasks
  set sw [winfo screenwidth $w]
  set sh [winfo screenheight $w]
  set maxw [expr {$sw - 30}]
  set maxh [expr {$sh - 100}]
  set n 0
  while {$n < 5 && ([winfo reqheight $w] > $maxh || [winfo reqwidth $w] > $maxw)} {
    if {![::rsp::shrink $w]} break
    incr n
    update idletasks
  }
  set rw [winfo reqwidth $w]
  set rh [winfo reqheight $w]
  if {![regexp {^([0-9]+)x([0-9]+)([+-]-?[0-9]+)([+-]-?[0-9]+)$} [wm geometry $w] -> cw ch xs ys]} return
  set xs [expr {int([string trimleft $xs +])}]
  set ys [expr {int([string trimleft $ys +])}]
  set x $xs
  set y $ys
  set nw $cw
  set nh $ch
  if {[info exists flag($w)]} {
    if {$rw > $nw} { set nw $rw }
    if {$rh > $nh} { set nh $rh }
  }
  if {$nw > $maxw} { set nw $maxw }
  if {$nh > $maxh} { set nh $maxh }
  if {$x + $nw > $sw} { set x [expr {$sw - $nw}] }
  if {$y + $nh > $sh - 60} { set y [expr {$sh - 60 - $nh}] }
  if {$x < 0} { set x 0 }
  if {$y < 0} { set y 0 }
  if {$nw != $cw || $nh != $ch || $x != $xs || $y != $ys} {
    wm geometry $w ${nw}x${nh}+${x}+${y}
  }
}
proc ::rsp::onmap {w} {
  if {![winfo exists $w]} return
  if {[winfo toplevel $w] ne $w} return
  after idle [list ::rsp::fit $w]
  foreach ms {150 600 1500 4000} { after $ms [list ::rsp::fit $w] }
}
bind RspTag <Destroy> {::rsp::gone %W}
if {[string first {::rsp::onmap} [bind Toplevel <Map>]] < 0} {
  bind Toplevel <Map> {+::rsp::onmap %W}
}
'

# Installs (once per R session) the Tcl hooks that adapt every window to the
# screen. Called when a window is created.
.rsp_install = function() {
  try(tcltk::.Tcl(.rsp_tcl_script), silent = TRUE)
  invisible(NULL)
}

# Replaces a fixed geometry: window size capped to the screen, horizontally
# centred and kept fully on-screen; content larger than the requested size
# will enlarge it (see ::rsp::fit).
.rsp_geometry = function(win, w, h) {
  .rsp_install()
  sw = suppressWarnings(as.integer(tcltk::tclvalue(tcltk::tkwinfo("screenwidth", win))))
  sh = suppressWarnings(as.integer(tcltk::tclvalue(tcltk::tkwinfo("screenheight", win))))
  if (is.na(sw) || is.na(sh)) {
    tcltk::tkwm.geometry(win, sprintf("%dx%d", as.integer(w), as.integer(h)))
    return(invisible(NULL))
  }
  w2 = min(as.integer(w), sw - 30L)
  h2 = min(as.integer(h), sh - 100L)
  x2 = max(0L, (sw - w2) %/% 2L)
  y2 = max(0L, (sh - 60L - h2) %/% 4L)
  tcltk::tkwm.geometry(win, sprintf("%dx%d+%d+%d", w2, h2, x2, y2))
  # The window may already be mapped (Map event missed before the hook existed).
  try(tcltk::.Tcl(sprintf("set ::rsp::flag(%s) 1; ::rsp::onmap %s", as.character(win$ID), as.character(win$ID))),
      silent = TRUE)
  invisible(NULL)
}
