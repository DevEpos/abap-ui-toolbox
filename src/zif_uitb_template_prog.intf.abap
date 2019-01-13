INTERFACE zif_uitb_template_prog
  PUBLIC .


  INTERFACES zif_uitb_view .
  INTERFACES zif_uitb_view_callback .

  ALIASES show
    FOR zif_uitb_view~show .

  CONSTANTS c_alv_pf_status TYPE sy-pfkey VALUE 'ALV_STANDARD' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF c_alv_ucomm_edit,
      refresh TYPE sy-ucomm VALUE '&REFRESH',
      check   TYPE sy-ucomm VALUE '&CHECK',
    END OF c_alv_ucomm_edit .
  CONSTANTS c_func_ok TYPE sy-ucomm VALUE 'OK' ##NO_TEXT.
  CONSTANTS c_func_save TYPE sy-ucomm VALUE 'SAVE' ##NO_TEXT.
  CONSTANTS c_func_choose TYPE sy-ucomm VALUE 'CHOOSE' ##NO_TEXT.
  CONSTANTS c_func_leave TYPE sy-ucomm VALUE '&F03' ##NO_TEXT.
  CONSTANTS c_func_leave_no_exit_event TYPE sy-ucomm VALUE '&EXIT&' ##NO_TEXT.
  CONSTANTS c_func_cancel TYPE sy-ucomm VALUE '&F12' ##NO_TEXT.
  CONSTANTS c_func_quit TYPE sy-ucomm VALUE '&F15' ##NO_TEXT.
  CONSTANTS c_func_find TYPE sy-ucomm VALUE 'SEARCH' ##NO_TEXT.
  CONSTANTS c_func_find_more TYPE sy-ucomm VALUE 'SEARCHMORE' ##NO_TEXT.
  CONSTANTS c_func_page_up TYPE sy-ucomm VALUE 'SCROLL_UP' ##NO_TEXT.
  CONSTANTS c_func_page_top TYPE sy-ucomm VALUE 'SCROLL_TOP' ##NO_TEXT.
  CONSTANTS c_func_page_down TYPE sy-ucomm VALUE 'SCROLL_DWN' ##NO_TEXT.
  CONSTANTS c_func_page_bottom TYPE sy-ucomm VALUE 'SCROLL_BTM' ##NO_TEXT.
  CONSTANTS c_func_f2 TYPE sy-ucomm VALUE 'CHOOSE' ##NO_TEXT.
  CONSTANTS c_func_f5 TYPE sy-ucomm VALUE 'FUNC2' ##NO_TEXT.
  CONSTANTS c_func_f6 TYPE sy-ucomm VALUE 'FUNC3' ##NO_TEXT.
  CONSTANTS c_func_f7 TYPE sy-ucomm VALUE 'FUNC4' ##NO_TEXT.
  CONSTANTS c_func_f8 TYPE sy-ucomm VALUE 'FUNC1' ##NO_TEXT.
  CONSTANTS c_func_f9 TYPE sy-ucomm VALUE 'FUNC19' ##NO_TEXT.
  CONSTANTS c_func_ctrl_f5 TYPE sy-ucomm VALUE 'FUNC5' ##NO_TEXT.
  CONSTANTS c_func_ctrl_f6 TYPE sy-ucomm VALUE 'FUNC6' ##NO_TEXT.
  CONSTANTS c_func_ctrl_f7 TYPE sy-ucomm VALUE 'FUNC7' ##NO_TEXT.
  CONSTANTS c_func_ctrl_f8 TYPE sy-ucomm VALUE 'FUNC8' ##NO_TEXT.
  CONSTANTS c_func_shift_f1 TYPE sy-ucomm VALUE 'FUNC9' ##NO_TEXT.
  CONSTANTS c_func_shift_f4 TYPE sy-ucomm VALUE 'FUNC10' ##NO_TEXT.
  CONSTANTS c_func_shift_f5 TYPE sy-ucomm VALUE 'FUNC11' ##NO_TEXT.
  CONSTANTS c_func_shift_f6 TYPE sy-ucomm VALUE 'FUNC12' ##NO_TEXT.
  CONSTANTS c_func_shift_f7 TYPE sy-ucomm VALUE 'FUNC13' ##NO_TEXT.
  CONSTANTS c_func_shift_f8 TYPE sy-ucomm VALUE 'FUNC14' ##NO_TEXT.
  CONSTANTS c_func_shift_f9 TYPE sy-ucomm VALUE 'FUNC15' ##NO_TEXT.
  CONSTANTS c_func_shift_f11 TYPE sy-ucomm VALUE 'FUNC27' ##NO_TEXT.
  CONSTANTS c_func_shift_f12 TYPE sy-ucomm VALUE 'FUNC28' ##NO_TEXT.
  CONSTANTS c_func_ctrl_f1 TYPE sy-ucomm VALUE 'FUNC16' ##NO_TEXT.
  CONSTANTS c_func_ctrl_f2 TYPE sy-ucomm VALUE 'FUNC17' ##NO_TEXT.
  CONSTANTS c_func_ctrl_f3 TYPE sy-ucomm VALUE 'FUNC18' ##NO_TEXT.
  CONSTANTS c_func_shift_f2 TYPE sy-ucomm VALUE 'FUNC20' ##NO_TEXT.
  CONSTANTS c_func_ctrl_f4 TYPE sy-ucomm VALUE 'FUNC21' ##NO_TEXT.
  CONSTANTS c_func_ctrl_shift_f4 TYPE sy-ucomm VALUE 'FUNC22' ##NO_TEXT.
  CONSTANTS c_func_ctrl_shift_f5 TYPE sy-ucomm VALUE 'FUNC23' ##NO_TEXT.
  CONSTANTS c_func_ctrl_shift_f6 TYPE sy-ucomm VALUE 'FUNC24' ##NO_TEXT.
  CONSTANTS c_func_ctrl_shift_f7 TYPE sy-ucomm VALUE 'FUNC25' ##NO_TEXT.
  CONSTANTS c_func_ctrl_shift_f8 TYPE sy-ucomm VALUE 'FUNC26' ##NO_TEXT.
  CONSTANTS c_save TYPE sy-ucomm VALUE 'SAVE' ##NO_TEXT.
  CONSTANTS c_search TYPE sy-ucomm VALUE 'SEARCH' ##NO_TEXT.
  CONSTANTS c_search_more TYPE sy-ucomm VALUE 'SEARCHMORE' ##NO_TEXT.
  CONSTANTS c_container_name TYPE dynfnam VALUE 'CONTAINER' ##NO_TEXT.
  CONSTANTS c_template_prog_id TYPE syrepid VALUE 'SAPLZUITB_GUI_TEMPLATE' ##NO_TEXT.
  CONSTANTS c_template_prog_dynnr TYPE sydynnr VALUE '0100' ##NO_TEXT.
  CONSTANTS c_template_prog_dialog_dynnr TYPE sydynnr VALUE '0101' ##NO_TEXT.
  CONSTANTS c_edit TYPE sy-ucomm VALUE 'EDIT' ##NO_TEXT.

  "! Returns the gui container of the screen
  "! @parameter rr_container | the container reference
  METHODS get_container
    RETURNING
      VALUE(rr_container) TYPE REF TO cl_gui_custom_container .
  "! Adds a dynamic function to the status bar
  "! @parameter iv_function_id | the id of the dynamic status bar e.g. FUNC1
  "! @parameter iv_text | text for the function
  "! @parameter iv_icon | icon for the function
  "! @parameter iv_icon_text |
  "! @parameter iv_quickinfo |
  METHODS add_function
    IMPORTING
      !iv_function_id TYPE sy-ucomm
      !iv_text        TYPE gui_text
      !iv_icon        TYPE icon_d OPTIONAL
      !iv_icon_text   TYPE gui_ictext OPTIONAL
      !iv_quickinfo   TYPE gui_info OPTIONAL .
  METHODS set_function_code DEFAULT IGNORE
    IMPORTING
      !iv_function TYPE sy-ucomm .
  METHODS leave_program DEFAULT IGNORE
    IMPORTING
      !if_prevent_exit_event TYPE abap_bool DEFAULT abap_true .
  METHODS add_control_to_lifecycle
    IMPORTING
      !ir_control TYPE REF TO cl_gui_control .
ENDINTERFACE.
