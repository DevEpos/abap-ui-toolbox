*&---------------------------------------------------------------------*
*& Report zuitb_test_alv
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zuitb_test_alv.

TYPES: BEGIN OF ty_s_data,
         plant TYPE werks_d,
         datab TYPE datab,
         datbi TYPE datbi,
       END OF ty_s_data.

DATA: gt_data TYPE STANDARD TABLE OF ty_s_data.

gt_data = VALUE #(
  ( plant = '1001' datab = '20180101' datbi = '20181231' )
  ( plant = '1001' datab = '20180101' datbi = '20181231' )
  ( plant = '1001' datab = '20180101' datbi = '20181231' )
  ( plant = '1001' datab = '20180101' datbi = '20181231' )
  ( plant = '1001' datab = '20180101' datbi = '20181231' )
).

CLASS cl_alv_tester DEFINITION.

  PUBLIC SECTION.

    METHODS test_dialog.
    METHODS test_modal_dialog.
  PRIVATE SECTION.
    METHODS on_function
          FOR EVENT function_chosen OF zcl_uitb_alv_events
      IMPORTING
          ev_function
          ev_tag.
ENDCLASS.

CLASS cl_alv_tester IMPLEMENTATION.

  METHOD on_function.
    IF ev_function = 'MODAL'.
      test_modal_dialog( ).
    ELSEIF ev_function = 'DIALOG'.
      test_dialog( ).
    ENDIF.
  ENDMETHOD.

  METHOD test_dialog.
    DATA(lo_alv_dialog) = zcl_uitb_alv=>create_alv(
      ir_data                 = REF #( gt_data )
      iv_display_type         = zif_uitb_c_alv_display_types=>dialog
    ).
    lo_alv_dialog->set_popup_dimensions(
        iv_top    = 0
        iv_left   = 0
        iv_right  = 500
        iv_bottom = 500
    ).
    lo_alv_dialog->get_display_settings( )->set_title( 'ALV in dialog' ).
    DATA(lo_functions) = lo_alv_dialog->get_functions( ).
    lo_functions->set_default( abap_true ).
    lo_functions->add_function(
        iv_name             = 'MODAL'
        iv_icon             = |{ icon_abap }|
        iv_text             = 'Call modal dialog'
    ).
    SET HANDLER: on_function FOR lo_alv_dialog->get_events( ).
    lo_alv_dialog->display( ).
  ENDMETHOD.

  METHOD test_modal_dialog.
    DATA(lo_alv_modal_dialog) = zcl_uitb_alv=>create_alv(
        ir_data                 = REF #( gt_data )
        iv_display_type         = zif_uitb_c_alv_display_types=>modal_dialog
      ).
    lo_alv_modal_dialog->set_popup_dimensions(
        iv_top    = 2
        iv_left   = 10
        iv_right  = 120
        iv_bottom = 20
    ).
    lo_alv_modal_dialog->get_display_settings( )->set_title( 'ALV in modal dialog' ).
    DATA(lo_functions) = lo_alv_modal_dialog->get_functions( ).
    lo_functions->set_default( abap_true ).
    lo_functions->add_function(
        iv_name             = 'DIALOG'
        iv_icon             = |{ icon_abap }|
        iv_text             = 'Call Dialog'
    ).
    SET HANDLER: on_function FOR lo_alv_modal_dialog->get_events( ).
    lo_alv_modal_dialog->display( ).
  ENDMETHOD.


ENDCLASS.

START-OF-SELECTION.
  NEW cl_alv_tester( )->test_modal_dialog( ).
