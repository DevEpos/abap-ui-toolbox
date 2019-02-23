"! <p class="shorttext synchronized" lang="en">Constants for GUI Screen</p>
INTERFACE zif_uitb_c_gui_screen
  PUBLIC .
  CONSTANTS:
    "! Constants for GUI Status functions
    BEGIN OF c_functions,
      ok                  TYPE sy-ucomm VALUE 'OK' ##NO_TEXT,
      save                TYPE sy-ucomm VALUE 'SAVE' ##NO_TEXT,
      leave               TYPE sy-ucomm VALUE '&F03' ##NO_TEXT,
      leave_no_exit_event TYPE sy-ucomm VALUE '&EXIT&' ##NO_TEXT,
      cancel              TYPE sy-ucomm VALUE '&F12' ##NO_TEXT,
      quit                TYPE sy-ucomm VALUE '&F15' ##NO_TEXT,
      page_up             TYPE sy-ucomm VALUE 'SCROLL_UP' ##NO_TEXT,
      page_top            TYPE sy-ucomm VALUE 'SCROLL_TOP' ##NO_TEXT,
      page_down           TYPE sy-ucomm VALUE 'SCROLL_DWN' ##NO_TEXT,
      page_bottom         TYPE sy-ucomm VALUE 'SCROLL_BTM' ##NO_TEXT,
      search              TYPE sy-ucomm VALUE 'SEARCH' ##NO_TEXT,
      search_more         TYPE sy-ucomm VALUE 'SEARCHMORE' ##NO_TEXT,
      f2                  TYPE sy-ucomm VALUE 'F2' ##no_text,
      f5                  TYPE sy-ucomm VALUE 'F5' ##no_text,
      f6                  TYPE sy-ucomm VALUE 'F6' ##no_text,
      f7                  TYPE sy-ucomm VALUE 'F7' ##no_text,
      f8                  TYPE sy-ucomm VALUE 'F8' ##no_text,
      f9                  TYPE sy-ucomm VALUE 'F9' ##no_text,
      shift_f1            TYPE sy-ucomm VALUE 'S_F1' ##no_text,
      shift_f2            TYPE sy-ucomm VALUE 'S_F2' ##no_text,
      shift_f4            TYPE sy-ucomm VALUE 'S_F4' ##no_text,
      shift_f5            TYPE sy-ucomm VALUE 'S_F5' ##no_text,
      shift_f6            TYPE sy-ucomm VALUE 'S_F6' ##no_text,
      shift_f7            TYPE sy-ucomm VALUE 'S_F7' ##no_text,
      shift_f8            TYPE sy-ucomm VALUE 'S_F8' ##no_text,
      shift_f9            TYPE sy-ucomm VALUE 'S_F9' ##no_text,
      shift_f11           TYPE sy-ucomm VALUE 'S_F11' ##no_text,
      shift_f12           TYPE sy-ucomm VALUE 'S_F12' ##no_text,
      ctrl_f1             TYPE sy-ucomm VALUE 'C_F1' ##no_text,
      ctrl_f2             TYPE sy-ucomm VALUE 'C_F2' ##no_text,
      ctrl_f3             TYPE sy-ucomm VALUE 'C_F3' ##no_text,
      ctrl_f4             TYPE sy-ucomm VALUE 'C_F4' ##no_text,
      ctrl_f5             TYPE sy-ucomm VALUE 'C_F5' ##no_text,
      ctrl_f6             TYPE sy-ucomm VALUE 'C_F6' ##no_text,
      ctrl_f7             TYPE sy-ucomm VALUE 'C_F7' ##no_text,
      ctrl_f8             TYPE sy-ucomm VALUE 'C_F8' ##no_text,
      ctrl_f9             TYPE sy-ucomm VALUE 'C_F9' ##no_text,
      ctrl_f10            TYPE sy-ucomm VALUE 'C_F10' ##no_text,
      ctrl_f11            TYPE sy-ucomm VALUE 'C_F11' ##no_text,
      ctrl_f12            TYPE sy-ucomm VALUE 'C_F12' ##no_text,
      ctrl_shift_f2       TYPE sy-ucomm VALUE 'C_S_F2' ##no_text,
      ctrl_shift_f3       TYPE sy-ucomm VALUE 'C_S_F3' ##no_text,
      ctrl_shift_f4       TYPE sy-ucomm VALUE 'C_S_F4' ##no_text,
      ctrl_shift_f5       TYPE sy-ucomm VALUE 'C_S_F5' ##no_text,
      ctrl_shift_f6       TYPE sy-ucomm VALUE 'C_S_F6' ##no_text,
      ctrl_shift_f7       TYPE sy-ucomm VALUE 'C_S_F7' ##no_text,
      ctrl_shift_f8       TYPE sy-ucomm VALUE 'C_S_F8' ##no_text,
      ctrl_shift_f9       TYPE sy-ucomm VALUE 'C_S_F9' ##no_text,
      ctrl_shift_f10      TYPE sy-ucomm VALUE 'C_S_F10' ##no_text,
      ctrl_shift_f11      TYPE sy-ucomm VALUE 'C_S_F11' ##no_text,
      ctrl_shift_f12      TYPE sy-ucomm VALUE 'C_S_F12' ##no_text,
    END OF c_functions.

  CONSTANTS c_shortcuts_function TYPE sy-ucomm VALUE 'SHORTCUTS' ##no_text.
  CONSTANTS c_container_name TYPE dynfnam VALUE 'CONTAINER' ##NO_TEXT.
  CONSTANTS c_report_id TYPE syrepid VALUE 'SAPLZUITB_GUI_FRAMEWORK' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF c_screen_id,
      normal TYPE sy-dynnr VALUE '0100',
      dialog TYPE sy-dynnr VALUE '0101',
    END OF c_screen_id.

ENDINTERFACE.
