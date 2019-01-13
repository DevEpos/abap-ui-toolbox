CLASS ZCL_UITB_editable_alv_popup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES ZIF_UITB_view.
    METHODS constructor
      IMPORTING
        ir_t_data TYPE REF TO data.
  PROTECTED SECTION.

    METHODS on_before_output FOR EVENT before_output
          OF ZIF_UITB_view_callback
      IMPORTING
          er_callback.
  PRIVATE SECTION.
    DATA mr_template_program TYPE REF TO ZIF_UITB_template_prog.
    DATA mr_alv TYPE REF TO ZCL_UITB_alv.
    DATA mr_t_data TYPE REF TO data.
    DATA mf_editable TYPE abap_bool.
    METHODS do_on_first_screen_call.
    METHODS do_before_show.
ENDCLASS.




CLASS ZCL_UITB_editable_alv_popup IMPLEMENTATION.
  METHOD constructor.
    mr_t_data = ir_t_data.
    " create template program
    mr_template_program = ZCL_UITB_templt_prog_callback=>create_template_program( 'ALV' ).

    " register event handlers
    SET HANDLER: on_before_output FOR mr_template_program.
  ENDMETHOD.

  METHOD on_before_output.
    IF er_callback->is_first_screen_call( ).
      do_on_first_screen_call( ).
    ENDIF.

    IF mf_editable = abap_false.
      er_callback->deactivate_function( ZIF_UITB_template_prog=>c_save ).
    ENDIF.

  ENDMETHOD.

  METHOD do_on_first_screen_call.
    mr_alv = ZCL_UITB_alv=>create_alv(
        ir_container = mr_template_program->get_container( )
        if_editable  = mf_editable
        ir_data      = mr_t_data
    ).
    IF mr_alv IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    DATA(lr_functions) = mr_alv->get_functions( ).
    lr_functions->set_all( abap_false ).
    lr_functions->set_function( zif_uitb_c_alv_functions=>local_append_row ).
    lr_functions->set_function( zif_uitb_c_alv_functions=>local_insert_row ).
    lr_functions->set_function( zif_uitb_c_alv_functions=>local_delete_row ).

    DATA(lr_columns) = mr_alv->get_columns( ).
    lr_columns->set_editable( mf_editable ).

    mr_alv->display( ).

  ENDMETHOD.

  METHOD do_before_show.
    mr_template_program->add_function(
        iv_function_id = ZIF_UITB_template_prog=>c_save
        iv_text        = 'Speichern'(002)
        iv_icon        = icon_system_save
      ).
  ENDMETHOD.

  METHOD ZIF_UITB_view~show.
    do_before_show( ).

    mr_template_program->ZIF_UITB_view~show(
        iv_start_line   = 2
        iv_start_column = 15
        iv_end_line     = 15
        iv_end_column   = 90
    ).
  ENDMETHOD.

ENDCLASS.
