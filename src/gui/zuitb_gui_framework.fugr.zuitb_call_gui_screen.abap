FUNCTION zuitb_call_gui_screen.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_PROGRAM_TITLE) TYPE  CUA_TIT_TX
*"     REFERENCE(IF_MODAL) TYPE  ABAP_BOOL OPTIONAL
*"     REFERENCE(IO_CALLBACK) TYPE REF TO  ZCL_UITB_GUI_DYNPRO_HANDLER
*"     REFERENCE(IV_START_LINE) TYPE  I OPTIONAL
*"     REFERENCE(IV_START_COLUMN) TYPE  I OPTIONAL
*"     REFERENCE(IV_END_LINE) TYPE  I OPTIONAL
*"     REFERENCE(IV_END_COLUMN) TYPE  I OPTIONAL
*"----------------------------------------------------------------------
  DATA(lv_screen_number) = COND sy-dynnr( WHEN if_modal = abap_true THEN '0200' ELSE '0100' ).

  " save current controller
  IF gs_view_data IS NOT INITIAL.
    INSERT gs_view_data INTO gt_view_controller INDEX 1.
    CLEAR gs_view_data.
  ENDIF.

  gs_view_data-controller = io_callback.
  gs_view_data-title = iv_program_title.
  gs_view_data-is_first_call = abap_true.

  IF iv_start_line IS NOT INITIAL.
    gs_view_data-as_dialog = abap_true.

    IF iv_end_column IS INITIAL OR iv_end_line IS INITIAL.
      CALL SCREEN lv_screen_number STARTING AT iv_start_column iv_start_line.
    ELSE.
      CALL SCREEN lv_screen_number STARTING AT iv_start_column iv_start_line ENDING AT iv_end_column iv_end_line.
    ENDIF.
  ELSE.
    CALL SCREEN lv_screen_number.
  ENDIF.

ENDFUNCTION.
