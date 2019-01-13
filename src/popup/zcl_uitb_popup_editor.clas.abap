class ZCL_UITB_POPUP_EDITOR definition
  public
  final
  create public .

public section.

  interfaces ZIF_UITB_VIEW .

  methods CONSTRUCTOR
    importing
      !IV_TEXT type STRING optional
      !IR_V_TEXT type ref to STRING optional
      !IV_EDITOR_TITLE type STRING optional
      !IF_EDITABLE type ABAP_BOOL optional
      !IF_CODING_FONT type ABAP_BOOL default ABAP_TRUE .
  methods WAS_SAVED
    returning
      value(RESULT) type ABAP_BOOL .
  PROTECTED SECTION.

    METHODS on_before_output FOR EVENT before_output
          OF zif_uitb_view_callback
      IMPORTING
          er_callback.

    METHODS on_user_command FOR EVENT user_command
          OF zif_uitb_view_callback
      IMPORTING
          er_callback
          ev_function_id.
  PRIVATE SECTION.
    DATA mr_template_program TYPE REF TO zif_uitb_template_prog.
    DATA mr_editor TYPE REF TO cl_gui_textedit.
    DATA mf_use_coding_font TYPE abap_bool.
    DATA mf_was_saved TYPE abap_bool.
    DATA mf_editable TYPE abap_bool.
    DATA mv_editor_text TYPE string.
    DATA mr_v_text TYPE REF TO string.
    METHODS do_on_first_screen_call.
    METHODS do_before_show.
ENDCLASS.



CLASS ZCL_UITB_POPUP_EDITOR IMPLEMENTATION.


  METHOD constructor.
    mf_editable = if_editable.
    mr_v_text = ir_v_text.
    mv_editor_text = iv_text.

    mf_use_coding_font = if_coding_font.
    " create template program
    mr_template_program = zcl_uitb_templt_prog_callback=>create_template_program(
      COND #( WHEN iv_editor_title IS NOT INITIAL THEN iv_editor_title ELSE 'Editor' )
    ).

    " register event handlers
    SET HANDLER:
        on_before_output FOR mr_template_program,
        on_user_command FOR mr_template_program.
  ENDMETHOD.


  METHOD do_before_show.
    mr_template_program->add_function(
      iv_function_id = zif_uitb_template_prog=>c_save
      iv_text        = 'Speichern'(002)
      iv_icon        = icon_check
    ).
  ENDMETHOD.


  METHOD do_on_first_screen_call.

    mr_editor = NEW #( parent = mr_template_program->get_container( ) ).
    IF mr_editor IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    mr_editor->set_comments_string( ).
    mr_editor->set_highlight_comments_mode( ).
    mr_editor->set_textstream( COND #( WHEN mr_v_text IS BOUND THEN mr_v_text->* ELSE mv_editor_text ) ).
    mr_editor->set_readonly_mode( COND #( WHEN mf_editable = abap_true THEN 0 ELSE 1 ) ).
    mr_editor->set_font_fixed( COND #( WHEN mf_use_coding_font = abap_true THEN 1 ELSE 0 ) ).
    mr_editor->set_selection_pos_in_line( line = 2 ).
  ENDMETHOD.


  METHOD on_before_output.
    IF er_callback->is_first_screen_call( ).
      do_on_first_screen_call( ).
    ENDIF.

    IF mf_editable = abap_false.
      er_callback->deactivate_function( zif_uitb_template_prog=>c_save ).
    ENDIF.

  ENDMETHOD.


  METHOD on_user_command.
    DATA lt_lines TYPE STANDARD TABLE OF text256.

    CASE ev_function_id.

      WHEN zif_uitb_template_prog=>c_save.
        IF mr_v_text IS BOUND.

          mr_editor->get_text_as_stream(
            IMPORTING text        = lt_lines
                      is_modified = DATA(lv_modified)

          ).

          IF lv_modified = cl_gui_textedit=>true.
            mf_was_saved = abap_true.
            mr_v_text->* = REDUCE #(
              INIT text = ``
              FOR line IN lt_lines
              NEXT text = |{ text }{ line }|
            ).
          ENDIF.
        ENDIF.

        er_callback->exit_screen( ).
    ENDCASE.
  ENDMETHOD.


  METHOD was_saved.
    result = mf_was_saved.
  ENDMETHOD.


  METHOD zif_uitb_view~show.
    do_before_show( ).

    mr_template_program->zif_uitb_view~show(
        iv_start_line   = COND #( WHEN iv_start_line IS INITIAL THEN 2 ELSE iv_start_line )
        iv_start_column = COND #( WHEN iv_start_column IS INITIAL THEN 15 ELSE iv_start_column )
        iv_end_line     = COND #( WHEN iv_end_line IS INITIAL THEN 25 ELSE iv_end_line )
        iv_end_column   = COND #( WHEN iv_end_column IS INITIAL THEN 120 ELSE iv_end_column )
    ).
  ENDMETHOD.
ENDCLASS.
