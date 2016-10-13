#!/usr/local/bin/wish
# plotWAT - plotting GUI
# Start: 04 July 2016
#source {C:\Tcl\projects\plotWAT\plotWAT.tcl}

global WorkDir os GpDir

#Determine OS
set os $tcl_platform(platform)
if {[string match unix $os]==1} {
	set BinDir [pwd]
	set GpDir {/usr/bin/}
} else {
	set BinDir [pwd]
	set WorkDir [pwd]
	set GpDir {C:/Program Files (x86)/gnuplot/bin}
}

#Set initial working directory
proc initworkdir {} {
	global WorkDir BinDir
	cd $BinDir
	if {[file exists "plotWAT_curdir"]} {
		set fid [open "plotWAT_curdir" r]
		set WorkDir [read $fid]
		catch "close $fid"
	} else {
		set WorkDir $BinDir
	}
}

#TODO: have welcoming message with short tutorial (include more thorough tutorial in help section)
#TODO: add a checkbox to welcoming message to disable it (if desired) on later runs of plotWAT

#plotWAT interface
proc main {} {
	global openf
	set openf "new_plot.gp"
	
	#main window
	wm title . "plotWAT: $openf"
	wm maxsize . 400 650
	wm minsize . 400 650
	
	set w .main
	frame $w
	pack $w -fill both -expand yes
	
	#Menubar config
	frame $w.menubar -relief raised
	pack $w.menubar -side top -fill x
	set main [menu .menu -tearoff false -type menubar]
	$main add cascade -menu .menu.file -label File -underline 0
		set filem [menu .menu.file -tearoff 0 -type normal]
		#TODO: reset application + new file
		#TODO: save current settings as a separate file
		#TODO: add pop-up window with general information about app
		$filem add command -label "New" -underline 0 -command {} 
		$filem add command -label "Open..." -underline 0 -command {loadData ""} 
		$filem add command -label "Save As" -underline 0 -command {} 
	$main add cascade -menu .menu.help -label Help -underline 0
		set helpm [menu .menu.help -tearoff 0 -type normal]
		$helpm add command -label "About plotWAT" -underline 0 -command {} 
	. configure -menu .menu
	
	#Notebook config
	ttk::notebook $w.nb
	set tab1 [frame $w.nb.frame1]
	set tab2 [frame $w.nb.frame2]
	$w.nb add $tab1 -text " Plot settings "
	$w.nb add $tab2 -text "Statistical Analysis"
	pack $w.nb -side top -padx 5 -fill both -expand true
	
	####################
	#TAB1: Plot settings
	####################
	
	#Functions
	set ffunctions [labelframe $tab1.functions -text "Function(s)"]
	pack $ffunctions -side top -fill both -padx 5 -expand true
	set ffleft [frame $ffunctions.l]
	set ffright [frame $ffunctions.r]
	ttk::separator $ffunctions.sep -orient vertical
	pack $ffleft $ffunctions.sep $ffright -side left -fill both -expand true
	
	#Function entries
	ttk::label $ffleft.lf1 -text "f1"
	ttk::entry $ffleft.ef1 -width 30
	grid $ffleft.lf1 $ffleft.ef1 -row 0 -sticky nw -padx 5 -pady 5
	
	ttk::label $ffleft.lf2 -text "f2"
	ttk::entry $ffleft.ef2 -width 30
	grid $ffleft.lf2 $ffleft.ef2 -sticky nw -padx 5 -pady 5
	$ffleft.ef2 state {disabled}
	# $ffunctions.ef1 instate {pressed !disabled} {label $ffunctions.lf2 -text "f2"; grid $ffunctions.lf2 -sticky nw}
	$ffleft.lf2 instate {active} {$ffleft.ef2 state {!disabled}}
	
	#Function buttons
	button $ffright.badd -text "Add" -height 1 -width 10 -command {}
	button $ffright.bclear -text "Clear" -height 1 -width 10 -command {}
	pack $ffright.badd $ffright.bclear -side top -pady 5
	
	
	#Labels and titles
	set flabel [labelframe $tab1.labels -text "Labels and titles"]
	pack $flabel -side top -fill both -padx 5 -expand true
	
	initworkdir
}

#Procedure to load DATA file
proc loadData {f} {
	global WorkDir BinDir openf
	
	set typelist {
		{{Gnuplot script} {*.gp}}
		{{All Files} {*.*}}
	}
	set openf [tk_getOpenFile -filetypes $typelist -initialdir $WorkDir	-title "Open file"]
	set WorkDir [file dirname $openf]
	wm title . "plotWAT: [file tail $openf]"; #Change window title accordingly
	cd $BinDir
	set fid [open "plotWAT_curdir" w]
	puts $fid $WorkDir
	catch "close $fid"
}

#Initializing Gnuplot pipe
proc GpInit {} {
	global gpid os GpDir WorkDir
    
	if {[string match unix $os]==1} {
		catch "set gpid [open |/opt/gnuplot/gnuplot w]"
	} else {
		cd $GpDir    
		catch "set gpid [open |gnuplot.exe w]"
		cd $WorkDir
	}
	sendGp "cd \'$WorkDir"
}

#Piping commands to Gnuplot
proc sendGp {cmd} {
  global gpid
  
  puts $gpid  $cmd
  flush $gpid
  #TODO: Add lines to keep track of commands in script file
}
main
