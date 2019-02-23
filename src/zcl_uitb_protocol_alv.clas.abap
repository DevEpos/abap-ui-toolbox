CLASS zcl_uitb_protocol_alv DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_fullscreen_alv_table
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !it_messages TYPE STANDARD TABLE .
  PROTECTED SECTION.
    METHODS: select_data REDEFINITION,
      get_report_id REDEFINITION,
      get_status REDEFINITION,
      on_user_command REDEFINITION,
      get_title REDEFINITION,
      adjust_columns REDEFINITION,
      adjust_functions REDEFINITION,
      get_table_reference REDEFINITION,
      adjust_display_settings REDEFINITION.
  PRIVATE SECTION.
    DATA mt_protocol TYPE STANDARD TABLE OF zuitb_protocol_info.
ENDCLASS.



CLASS zcl_uitb_protocol_alv IMPLEMENTATION.


  METHOD adjust_columns.
    DATA: lr_column TYPE REF TO cl_salv_column.

    lr_column ?= ir_columns->get_column( 'TYPE_ICON' ).
    lr_column->set_short_text( |{ 'Type'(001) }| ).
    lr_column->set_medium_text( space ).
    lr_column->set_long_text( space ).

    lr_column ?= ir_columns->get_column( 'MESSAGE' ).
    lr_column->set_short_text( space ).
    lr_column->set_medium_text( |{ 'Message'(002) }| ).
    lr_column->set_long_text( space ).
  ENDMETHOD.


  METHOD adjust_functions.
    super->adjust_functions( ir_functions = ir_functions ).

    IF mf_show_as_dialog = abap_true.
      ir_functions->set_all( abap_false ).
    ELSEIF mf_show_docked = abap_true.
      ir_functions->set_all( abap_false ).
      ir_functions->add_function(
         name     = 'CLOSE'
         icon     = '@3X@'
         tooltip  = 'Close'
         position =  if_salv_c_function_position=>right_of_salv_functions   " Funktion Positionierung
      ).
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    mt_protocol = it_messages.
  ENDMETHOD.


  METHOD get_report_id.
    rv_repid = 'SAPLZUITB_GUI_TEMPLATE'.
  ENDMETHOD.


  METHOD get_status.
    rv_status = 'PROTOCOL'.
  ENDMETHOD.


  METHOD get_table_reference.
    rr_table_ref = REF #( mt_protocol ).
  ENDMETHOD.


  METHOD get_title.
    rv_title = 'Messages'.
  ENDMETHOD.


  METHOD on_user_command.
    IF e_salv_function = 'CLOSE'.
      close( ).
    ENDIF.
  ENDMETHOD.


  METHOD select_data ##needed.
    " nothing to select
  ENDMETHOD.

  METHOD adjust_display_settings.
    ir_display_settings->set_list_header_size( value = cl_salv_display_settings=>c_header_size_small ).
  ENDMETHOD.

ENDCLASS.
