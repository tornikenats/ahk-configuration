﻿#SingleInstance force
;~ OutputDebug DBGVIEWCLEAR

GoSub, ConfigMinimizeToTray

sqas := new SquAeroSnap()
return

;~ ^Esc::
GuiClose:
ExitApp
; ========================= Concepts used in this code ================================

; == Axes, Vectors and Edges ==
; Moving occurs along an axis
;	If you hit left or right, that's a move along the x axis
;	If you hit up/down, that's that's a move along the y axis
; Movement is in the direction of a vector
;	-1 is towards the origin, so left or up
;	+1 is away from the origin, so right or down
;
; Resizing operates upon an edge, along an axis in the direction of a vector.
; eg the right edge moves to the right, sizing up the window horizontally
; = +1 edge of x axis moves in vector +1
; It is initiated by holding an additional modifier when you press an arrow key

; === Monitor Index (ID) and Order ===
; AHK gives each monitor an Index (Starting with 1, counting up)
; These Indexes however are not guaranteed to be in the same order as they are physically arranged
; Monitor "Order" is the physical order they are arranged in (1 being the left-most)

; === Pos and Span ===
; A given window has Pos and Span attributes for each axis
; Pos is the position of the winow along that axis: 1 is the left/top-most tile
; Span is how many tiles that window covers along that axis

class SquAeroSnap {
    MonitorOrder := []
	MonitorRows := 2
	MonitorCols := 2
    Monitors := []
    TiledWindows := {}
	IgnoreFirstMove := 0
	
	Axes := {x: 1, y: 2}
	AxisToWh := {x: "w", y: "h"}
    
    __New(){
		this.IniFile := RegExReplace(A_ScriptName, "\.exe|\.ahk", ".ini")

        ; Gui, +hwndhwnd
        ; this.hwnd := hwnd
		
		; ; === Gui ===
		; Gui, Add, GroupBox, w250 h75 Center Section, General Settings

		; ; -- Rows --
		; Gui, Add, Text, xs+20 yp+25 w50, Rows
		; Gui, Add, Edit, x+5 yp-3 w40 hwndhRowsEdit
		; this.hRowsEdit := hRowsEdit
        
		; ; -- Columns --
		; Gui, Add, Text, x+20 yp+3 w50, Columns
		; Gui, Add, Edit, x+5 yp-3 w40 hwndhColsEdit
		; this.hColsEdit := hColsEdit

		; ; -- Ignore first move --
		; Gui, Add, CheckBox, % "xs+20 y+10 hwndhIgnoreFirstmove AltSubmit", Ignore first move or size, just snap to tile(s)
		; this.hIgnoreFirstmove := hIgnoreFirstmove
		
		; ; -- Instructions --
		; Gui, Add, GroupBox, xm y+20 w250 h140 Center, Hotkeys
		; Gui, Add, Text, xp+1 yp+25 w245 R8 hwndhHotkeyInstructions Center
		; this.hHotkeyInstructions := hHotkeyInstructions
		
		; === Initialize Monitors ===
        SysGet, MonitorCount, MonitorCount
        this.MonitorCount := MonitorCount
        
        Loop % this.MonitorCount {
            this.Monitors.push(new this.CMonitor(A_Index))
        }

        if (this.MonitorOrder.length() != this.MonitorCount){
			this.SetupMonitorLayout()
        }
		this.UpdateMonitorTileConfiguration()
		
		; === Load Settings ===
		settings_loaded := this.LoadSettings()
		Gui, Show, Hide, SquAeroSnap

		; === Minimze to tray if not first run, else show Gui ===
		if (!settings_loaded){
			GoSub, OnMinimizeButton
		} else {
			Gui, Show
		}
		
		; === Enable GuiControl Callbacks ===
		this.SetGuiControlCallbackState(1)
    }
	
	; Enables / Disables callbacks for GuiControls
	SetGuiControlCallbackState(state){
		if (state){
			fn := this.RowColChanged.Bind(this)
			GuiControl, +g, % this.hRowsEdit, % fn
			GuiControl, +g, % this.hColsEdit, % fn

			fn := this.IgnoreFirstMoveChanged.Bind(this)
			GuiControl, +g, % this.hIgnoreFirstmove, % fn
		} else {
			GuiControl, -g, % this.hRowsEdit
			GuiControl, -g, % this.hColsEdit
			GuiControl, -g, % hIgnoreFirstmove
		}
	}
	
	; --------------------------------- Inital Setup -------------------------------
	LoadSettings(){
		hotkey_mode := 1
		if (FileExist(this.IniFile)){
			first_run := 0
		} else {
			first_run := 1
			FileAppend, % "", % this.IniFile
		}
		if (!first_run){
			IniRead, MonitorRows, % this.IniFile, Settings, MonitorRows
			if (MonitorRows != "ERROR"){
				this.MonitorRows := MonitorRows
			}
			IniRead, MonitorCols, % this.IniFile, Settings, MonitorCols
			if (MonitorCols != "ERROR"){
				this.MonitorCols := MonitorCols
			}
			IniRead, IgnoreFirstMove, % this.IniFile, Settings, IgnoreFirstMove
			if (IgnoreFirstMove != "ERROR"){
				this.IgnoreFirstMove := IgnoreFirstMove
			}
		}
		
		; Initialize hotkeys
		this.SetHotkeyState()
		
		; Update the GuiControls
		GuiControl, , % this.hRowsEdit, % this.MonitorRows
		GuiControl, , % this.hColsEdit, % this.MonitorCols
		GuiControl, , % this.hIgnoreFirstmove, % this.IgnoreFirstMove
		
		return first_run
	}
	
	; Turns On or Off hotkeys, or sets the mode
    SetHotkeyState(){
		fn := this.MoveSizeSpecific.Bind(this, 1, 1, 1, 1)
		hotkey, #a, % fn
		
		fn := this.MoveSizeSpecific.Bind(this, 2, 1, 1, 1)
		hotkey, #s, % fn
		
		fn := this.MoveSizeSpecific.Bind(this, 1, 2, 1, 1)
		hotkey, #z, % fn
		
		fn := this.MoveSizeSpecific.Bind(this, 2, 2, 1, 1)
		hotkey, #x, % fn

        fn := this.MoveSizeSpecific.Bind(this, 1, 1, 1, 2)
		hotkey, #q, % fn

        fn := this.MoveSizeSpecific.Bind(this, 2, 1, 1, 2)
		hotkey, #w, % fn

        fn := this.MoveSizeSpecific.Bind(this, 1, 1, 2, 1)
		hotkey, #d, % fn

        fn := this.MoveSizeSpecific.Bind(this, 1, 2, 2, 1)
		hotkey, #c, % fn
        
        fn := this.MoveSizeSpecific.Bind(this, 1, 1, 2, 2)
		hotkey, #Space, % fn

; -------------------------------------------------*----*

		fn := this.MoveWindow.Bind(this, "x", 1)
		hotkey, #Right, % fn
		
		fn := this.MoveWindow.Bind(this, "x", -1)
		hotkey, #Left, % fn
		
		fn := this.MoveWindow.Bind(this, "y", 1)
		hotkey, #Down, % fn
		
		fn := this.MoveWindow.Bind(this, "y", -1)
		hotkey, #Up, % fn
		
		fn := this.SizeWindow.Bind(this, "x", 1, 1)
		hotkey, #^Right, % fn
		
		fn := this.SizeWindow.Bind(this, "x", 1, -1)
		hotkey, #^Left, % fn
		
		fn := this.SizeWindow.Bind(this, "x", -1, 1)
		hotkey, #+Right, % fn
		
		fn := this.SizeWindow.Bind(this, "x", -1, -1)
		hotkey, #+Left, % fn
		
		fn := this.SizeWindow.Bind(this, "y", 1, 1)
		hotkey, #^Down, % fn
		
		fn := this.SizeWindow.Bind(this, "y", 1, -1)
		hotkey, #^Up, % fn
		
		fn := this.SizeWindow.Bind(this, "y", -1, 1)
		hotkey, #+Down, % fn
		
		fn := this.SizeWindow.Bind(this, "y", -1, -1)
		hotkey, #+Up, % fn
		
		this.SetHotkeyInstructions()
    }
	
	; Updates the Gui to show hotkeys
	SetHotkeyInstructions(){
		text =
		(
Base modifier of WIN to Move
Add Ctrl to Resize bottom right corner
Add Shift to Resize top left corner

WIN + Arrow Keys = Move Window
WIN + CTRL + Up/Down = Resize bottom edge
WIN + CTRL + Left/Right = Resize right edge
WIN + SHIFT + Up/Down = Resize top edge
WIN + SHIFT + Left/Right = Resize left edge
		)
		GuiControl, , % this.hHotkeyInstructions, % text
	}
    
	; Called on startup to work out physical layout of monitors
    SetupMonitorLayout(){
		tmp := {}
		for i, mon in this.Monitors {
			tmp[mon.Coords.l] := i
		}
		for i, id in tmp {
			this.MonitorOrder.push(id)
		}
	}
	
	; Instruct all monitors to pre-calculate their tile locations
	UpdateMonitorTileConfiguration(){
		for i, mon in this.Monitors {
			mon.SetRows(this.MonitorRows)
			mon.SetCols(this.MonitorCols)
		}
	}
	
	; ------------------------------- Window placement, movement and sizing ------------------------------
	; Called when a hotkey is hit to detect the current window
    GetWindow(){
        hwnd := WinExist("A")
        if (this.TiledWindows.HasKey(hwnd)){
			win := this.TiledWindows[hwnd]
		} else {
			win := new this.CWindow(hwnd)
        }
        return win
    }

	; Initializes a window if needed.
	; Returns 1 to indicate that the window is new
	InitWindow(win){
        if (this.TiledWindows.HasKey(win.hwnd)){
			return 0
		} else {
            this.TiledWindows[win.hwnd] := win
			win.CurrentMonitor := this.Monitors[this.GetWindowMonitor(win)]
			this.FitWindowToTiles(win)
			return 1
		}
	}
	
	; Works out initial placement for a window
	FitWindowToTiles(win){
		mon := win.CurrentMonitor
		coords := win.GetLocalCoords()

		for axis, unused in this.Axes {
			w_h := this.AxisToWh[axis] ; convert "x" or "y" to "w" or "h"
			; Work out initial position
			tile_pos := floor(coords[axis] / mon.TileSizes[axis]) + 1
			win.Pos[axis] := tile_pos
			
			; Work out how many tiles this window would fill if tiled
			num_tiles := floor(coords[w_h] / mon.TileSizes[axis])
			num_tiles := num_tiles ? num_tiles : 1	; minimum tile size of 1
			win.Span[axis] := num_tiles
			
			; Clamp window to max of full width of the axis
			if (win.Span[axis] > mon.TileCount[axis]){
				win.Span[axis] := mon.TileCount[axis]
			}
			
			; If window would extend off-screen on this axis, move it towards the origin
			sizediff := ((win.Pos[axis] + win.Span[axis]) - mon.TileCount[axis]) - 1
			if (sizediff > 0){
				win.Pos[axis] -= sizediff
			}
		}
		this.TileWindow(win)
	}

	; Moves a window along a specified axis in the direction of a specified vector
    MoveWindow(axis, vector){
        win := this.GetWindow()
		
		if (this.InitWindow(win) && this.IgnoreFirstMove){
			return
		}
		mon := win.CurrentMonitor
		
		new_pos := win.Pos[axis] + vector
		
		if ((new_pos + win.Span[axis] - 1) > mon.TileCount[axis]){
			if (axis == "y")
				return
			new_pos := 1
			mon := this.GetNextMonitor(mon.id, vector)
			win.CurrentMonitor := mon
		} else if (new_pos <= 0){
			if (axis == "y")
				return
			mon := this.GetNextMonitor(mon.id, vector)
			win.CurrentMonitor := mon
			new_pos := (mon.TileCount[axis] - win.Span[axis]) + (vector * -1)
		}
        Win.Pos[axis] := new_pos
        this.TileWindow(win)
    }
    
	; Sizes a window by moving and edge along a specific axis in the direction of a specified vector
    MoveSizeSpecific(x, y, w, h){
        win := this.GetWindow()
		
		if (this.InitWindow(win) && this.IgnoreFirstMove){
			return
		}

		mon := win.CurrentMonitor
		
        win.Span.x := w
        win.Span.y := h
        win.Pos.x := x
        win.Pos.y := y

        this.TileWindow(win)
    }

	; Sizes a window by moving and edge along a specific axis in the direction of a specified vector
    SizeWindow(axis, edge, vector){
        win := this.GetWindow()
		
		if (this.InitWindow(win) && this.IgnoreFirstMove){
			return
		}

		mon := win.CurrentMonitor
		
		new_pos := win.Pos[axis], new_span := win.Span[axis]
		
		if (edge == -1){
			; Change in span causes change in pos
			if ((vector == 1 && win.Span[axis] != 1) || (vector == -1 && win.Pos[axis] != 1)){
				new_span += (vector * -1)
				new_pos += vector
			}
		} else {
			new_span += (vector * edge)
		}
		if ((new_span == 0) || ((new_pos + new_span - 1) > mon.TileCount[axis])){
			return
		}
       
		;~ OutputDebug % "AHK| SIZE - Axis: " axis ", Edge: " edge ", Vector: " vector " / New Span: " new_span ", New Pos: " new_pos
		
		win.Span[axis] := new_span, win.Pos[axis] := new_pos
		
        this.TileWindow(win)
    }
	
	; Request a window be placed in it's designated tile
	TileWindow(win){
        mon := win.CurrentMonitor
		
		x := mon.TileCoords.x[win.Pos.x]
		y := mon.TileCoords.y[win.Pos.y]
		w := (mon.TileSizes.x * win.Span.x)
		h := (mon.TileSizes.y * win.Span.y)
		
		; If window is minimized or maximized, restore
		WinGet, MinMax, MinMax, % "ahk_id " win.hwnd
		if (MinMax != 0)
			WinRestore, % "ahk_id " win.hwnd

		;~ WinMove, % "ahk_id " win.hwnd, , x, y, w, h
		Window_move(win.hwnd, x, y, w, h)
		;~ OutputDebug % "AHK| Window Tile - PosX: " win.Pos.x ", PosY: " win.Pos.y ", SpanCols: " win.Span.x ", SpanRows: " win.Span.y
		;~ OutputDebug % "AHK| Window Coords - X: " x ", Y: " y ", W: " w ", H: " h
	}

	; -------------------------- Helper Functions ----------------------------
	; Returns a monitor object in a given vector
	; curr = Monitor ID (AHK monitor #)
	; vector = direction to look in
	; Returns monitor Object
	GetNextMonitor(curr, vector){
		found := 0
		for i, monid in this.MonitorOrder {
			if (monid == curr){
				found := 1
				break
			}
		}
		if (!found)
			return curr
		i += vector
		if (i > this.MonitorCount)
			i := 1
		else if (i < 1)
			i := this.MonitorCount
		return this.Monitors[this.MonitorOrder[i]]
	}

	; Takes a Monitor ID (AHK Monitor ID)
	; Returns a Monitor ORDER (Monitor 1 = LeftMost)
	GetMonitorOrder(mon){
		found := 0
		for i, monid in this.MonitorOrder {
			if (monid == mon){
				found := 1
				break
			}
		}
		if (found){
			return i
		} else {
			return mon
		}
	}
	
	; Returns the Monitor Index that the center of the window is on
	GetWindowMonitor(window){
		c := window.GetCenter()
		Loop % this.monitors.length() {
			m := this.monitors[A_Index].coords
			; Is the top-left corner of the window on this monitor?
			if (c.x >= m.l && c.x <= m.r && c.y >= m.t && c.y <= m.b){
				return A_Index
			}
		}
		return 0
	}
	
	; ------------------------- GuiControl handling ----------------------------------
	RowColChanged(){
		GuiControlGet, rows, , % this.hRowsEdit
		this.MonitorRows := rows
		GuiControlGet, cols, , % this.hColsEdit
		this.MonitorCols := cols
		IniWrite, % this.MonitorRows, % this.IniFile, Settings, MonitorRows
		IniWrite, % this.MonitorCols, % this.IniFile, Settings, MonitorCols
		this.UpdateMonitorTileConfiguration()
	}
	
	IgnoreFirstMoveChanged(){
		GuiControlGet, setting, , % this.hIgnoreFirstMove
		this.IgnoreFirstMove := setting
		IniWrite, % this.IgnoreFirstMove, % this.IniFile, Settings, IgnoreFirstMove
	}

	; -------------------------------------------- Monitor Class ---------------------------------
    class CMonitor {
		ID := 0 ;	The Index (AHK Monitor ID) of the monitor
        TileCoords := {x: [], y: []}
        TileSizes := {x: 0, y: 0}
        TileCount := {x: 2, y: 2}
        
        __New(id){
            this.id := id
            this.Coords := this.GetWorkArea()
        }
        
        SetRows(rows){
			this.TileCoords.y := []
            this.TileCount.y := rows
            this.TileSizes.y := round(this.Coords.h / rows)
            o := this.coords.t
            Loop % rows {
                this.TileCoords.y.push(o)
                o += this.TileSizes.y
            }
        }
        
        SetCols(cols){
			this.TileCoords.x := []
            this.TileCount.x := cols
            this.TileSizes.x := round(this.Coords.w / cols)
            o := this.coords.l
            Loop % cols {
                this.TileCoords.x.push(o)
                o += this.TileSizes.x
            }
        }
        
        ; Gets the "Work Area" of a monitor (The coordinates of the desktop on that monitor minus the taskbar)
        ; also pre-calculates a few values derived from the coordinates
        GetWorkArea(){
            SysGet, coords_, MonitorWorkArea, % this.id
            out := {}
            out.l := coords_left
            out.r := coords_right
            out.t := coords_top
            out.b := coords_bottom
            out.w := coords_right - coords_left
            out.h := coords_bottom - coords_top
            out.cx := coords_left + round(out.w / 2)	; center x
            out.cy := coords_top + round(out.h / 2)		; center y
            out.hw := round(out.w / 2)	; half width
            out.hh := round(out.w / 2)	 ; half height
            return out
        }
    }
    
	; ----------------------------------- Window Class ----------------------------------
    class CWindow {
        CurrentMonitor := 0	; Will point to monitor OBJECT when this window is tiled
        Pos := {x: 1, y: 1}
        Span := {x: 1, y: 1}
        
		AxisToOriginEdge := {x: "l", y: "t"}
		Axes := {x: 1, y: 2}

        __New(hwnd){
            this.hwnd := hwnd
        }
        
		GetCoords(){
			WinGetPos, wx, wy, ww, wh, % "ahk_id " this.hwnd
			return {x: wx, y: wy, w: ww, h: wh}
		}
		
		GetLocalCoords(){
			coords := this.GetCoords()
			wa := this.CurrentMonitor.GetWorkArea()
			for axis, unused in this.Axes {
				l_t := this.AxisToOriginEdge[axis]
				coords[axis] := abs(wa[l_t] - coords[axis])
			}
			;~ coords.x := mon.coords.x - coords.x, coords.x := mon.coords.y - coords.y, coords.x := mon.coords.w - coords.w, coords.x := mon.coords.h - coords.h
			return coords
		}
		
        ; Gets the coordinates of the center of the window
        GetCenter(){
			w := this.GetCoords()
            cx := w.x + round(w.w / 2)
            cy := w.y + round(w.h / 2)
            return {x: cx, y: cy}
        }
    }
}

; Code from Bug.n
; https://github.com/fuhsjr00/bug.n/blob/master/src/Window.ahk#L247

;; 0 - Not hung
;; 1 - Hung
Window_isHung(wndId) {
	static WM_NULL := 0
	detectHidden := A_DetectHiddenWindows
	DetectHiddenWindows, On
	SendMessage, WM_NULL, , , , % "ahk_id " wndId
	result := ErrorLevel
	DetectHiddenWindows, % detectHidden
	
	return result == 1
}

Window_getPosEx(hWindow, ByRef X = "", ByRef Y = "", ByRef Width = "", ByRef Height = "", ByRef Offset_X = "", ByRef Offset_Y = "") {
	Static Dummy5693, RECTPlus, S_OK := 0x0, DWMWA_EXTENDED_FRAME_BOUNDS := 9

	;-- Workaround for AutoHotkey Basic
	PtrType := (A_PtrSize=8) ? "Ptr" : "UInt"

	;-- Get the window's dimensions
	;   Note: Only the first 16 bytes of the RECTPlus structure are used by the
	;   DwmGetWindowAttribute and GetWindowRect functions.
	VarSetCapacity(RECTPlus, 24,0)
	DWMRC := DllCall("dwmapi\DwmGetWindowAttribute"
		,PtrType,hWindow                                ;-- hwnd
		,"UInt",DWMWA_EXTENDED_FRAME_BOUNDS             ;-- dwAttribute
		,PtrType,&RECTPlus                              ;-- pvAttribute
		,"UInt",16)                                     ;-- cbAttribute

	If (DWMRC != S_OK) {
		If ErrorLevel in -3, -4   ;-- Dll or function not found (older than Vista)
		{
			;-- Do nothing else (for now)
		} Else {
			outputdebug,
				(LTrim Join`s
				Function: %A_ThisFunc% -
				Unknown error calling "dwmapi\DwmGetWindowAttribute".
				RC = %DWMRC%,
				ErrorLevel = %ErrorLevel%,
				A_LastError = %A_LastError%.
				"GetWindowRect" used instead.
				)

			;-- Collect the position and size from "GetWindowRect"
			DllCall("GetWindowRect", PtrType, hWindow, PtrType, &RECTPlus)
		}
	}

	;-- Populate the output variables
	X := Left :=NumGet(RECTPlus, 0, "Int")
	Y := Top  :=NumGet(RECTPlus, 4, "Int")
	Right     :=NumGet(RECTPlus, 8, "Int")
	Bottom    :=NumGet(RECTPlus, 12, "Int")
	Width     :=Right-Left
	Height    :=Bottom-Top
	OffSet_X  := 0
	OffSet_Y  := 0

	;-- If DWM is not used (older than Vista or DWM not enabled), we're done
	If (DWMRC <> S_OK)
		Return &RECTPlus

	;-- Collect dimensions via GetWindowRect
	VarSetCapacity(RECT, 16, 0)
	DllCall("GetWindowRect", PtrType, hWindow, PtrType, &RECT)
	GWR_Width := NumGet(RECT, 8, "Int") - NumGet(RECT, 0, "Int")    ;-- Right minus Left
	GWR_Height := NumGet(RECT, 12, "Int") - NumGet(RECT, 4, "Int")  ;-- Bottom minus Top

	;-- Calculate offsets and update output variables
	NumPut(Offset_X := (Width  - GWR_Width)  // 2, RECTPlus, 16, "Int")
	NumPut(Offset_Y := (Height - GWR_Height) // 2, RECTPlus, 20, "Int")
	Return &RECTPlus
}

Window_move(wndId, x, y, width, height) {
	static WM_ENTERSIZEMOVE = 0x0231, WM_EXITSIZEMOVE  = 0x0232

	;~ If Not wndId Window_getPosEx(wndId, wndX, wndY, wndW, wndH) And (Abs(wndX - x) < 2 And Abs(wndY - y) < 2 And Abs(wndW - width) < 2 And Abs(wndH - height) < 2)
		;~ Return, 0
	addr := Window_getPosEx(wndId, wndX, wndY, wndW, wndH)
	if (!(wndId) && !(addr) &&  (Abs(wndX - x) < 2) &&  (Abs(wndY - y) < 2) &&  (Abs(wndW - width) < 2) &&  (Abs(wndH - height) < 2))
		return 0

	If Window_isHung(wndId) {
		OutputDebug % "DEBUG[2] Window_move: Potentially hung window " . wndId
		Return 1
	}
	/* Else {
		WinGet, wndMinMax, MinMax, % "ahk_id " wndId
		If (wndMinMax = -1 And Not Window_#%wndId%_isMinimized)
			WinRestore, ahk_id %wndId%
	}
	*/

	SendMessage, WM_ENTERSIZEMOVE, , , , % "ahk_id " wndId
	If ErrorLevel {
		;~ Debug_logMessage("DEBUG[2] Window_move: Potentially hung window " . wndId, 1)
		Return 1
	} Else {
		WinMove, % "ahk_id " wndId, , % x, % y, % width, % height
	
		;If Not (wndMinMax = 1) Or Not Window_#%wndId%_isDecorated Or Manager_windowNotMaximized(width, height) {
			If (Window_getPosEx(wndId, wndX, wndY, wndW, wndH) && (Abs(wndX - x) > 1 || Abs(wndY - y) > 1 || Abs(wndW - width) > 1 || Abs(wndH - height) > 1)) {
				x -= wndX - x
				y -= wndY - y
				width  += width - wndW - 1
				height += height - wndH - 1
				WinMove, % "ahk_id " wndId, , % x, % y, % width, % height
			}
		;}
	
		SendMessage, WM_EXITSIZEMOVE, , , , % "ahk_id " wndId
		Return, 0
	}
}


; Minimze to tray by SKAN http://www.autohotkey.com/board/topic/32487-simple-minimize-to-tray/

ConfigMinimizeToTray:
	Gui, +hwndGui1
	Menu("Tray","Nostandard"), Menu("Tray","Add","Restore","GuiShow"), Menu("Tray","Add")
	Menu("Tray","Default","Restore"), Menu("Tray","Click",1), Menu("Tray","Standard")
	OnMessage(0x112, "WM_SYSCOMMAND")

WM_SYSCOMMAND(wParam){
	If ( wParam = 61472 ) {
		SetTimer, OnMinimizeButton, -1
		Return 0
	}
}

Menu( MenuName, Cmd, P3="", P4="", P5="" ) {
	Menu, %MenuName%, %Cmd%, %P3%, %P4%, %P5%
	Return errorLevel
}

OnMinimizeButton:
	MinimizeGuiToTray( R, Gui1 )
	Menu("Tray","Icon")
	Return

GuiShow:
	DllCall("DrawAnimatedRects", UInt,Gui1, Int,3, UInt,&R+16, UInt,&R )
	Menu("Tray","NoIcon")
	Gui, Show
	Return

MinimizeGuiToTray( ByRef R, hGui ) {
	WinGetPos, X0,Y0,W0,H0, % "ahk_id " (Tray:=WinExist("ahk_class Shell_TrayWnd"))
	ControlGetPos, X1,Y1,W1,H1, TrayNotifyWnd1,ahk_id %Tray%
	SW:=A_ScreenWidth,SH:=A_ScreenHeight,X:=SW-W1,Y:=SH-H1,P:=((Y0>(SH/3))?("B"):(X0>(SW/3))
	? ("R"):((X0<(SW/3))&&(H0<(SH/3)))?("T"):("L")),((P="L")?(X:=X1+W0):(P="T")?(Y:=Y1+H0):)
	VarSetCapacity(R,32,0), DllCall( "GetWindowRect",UInt,hGui,UInt,&R)
	NumPut(X,R,16), NumPut(Y,R,20), DllCall("RtlMoveMemory",UInt,&R+24,UInt,&R+16,UInt,8 )
	DllCall("DrawAnimatedRects", UInt,hGui, Int,3, UInt,&R, UInt,&R+16 )
	WinHide, ahk_id %hGui%
}