CLASS zcl_uitb_generic_table DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_table .
    INTERFACES zif_uitb_page_scroller .

    METHODS validate ABSTRACT
      CHANGING
        !cv_function_code TYPE sy-ucomm .
  PROTECTED SECTION.
    DATA mv_linecount TYPE sy-tabix .
    DATA mv_looplines TYPE sy-loopc .
    DATA mv_current_line TYPE sy-tabix .
    DATA mr_table_data TYPE REF TO data.
    DATA mr_current_line TYPE REF TO data.
    DATA mr_tablecontrol TYPE REF TO cxtab_control.

    METHODS is_empty
      RETURNING
        VALUE(result) TYPE abap_bool.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_uitb_generic_table IMPLEMENTATION.

  METHOD zif_uitb_table~get_current_line_ref.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    zif_uitb_table~determine_current_line( ).

    IF mv_current_line = 0.
      RETURN.
    ENDIF.

    ASSIGN mr_table_data->* TO <lt_table>.
    rr_line = REF #( <lt_table>[ mv_current_line ] ).
  ENDMETHOD.

  METHOD zif_uitb_page_scroller~scroll_page_bottom.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    ASSIGN mr_table_data->* TO <lt_table>.

    mr_tablecontrol->top_line = mv_linecount - mv_looplines + 1.
    IF mr_tablecontrol->top_line < 1.
      mr_tablecontrol->top_line = 1.
    ENDIF.

    " set cursor to last line
    DATA(lv_lastindex) = mv_looplines.
    IF lv_lastindex > lines( <lt_table> ).
      lv_lastindex = lines( <lt_table> ).
    ENDIF.

    zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_line( lv_lastindex ).

  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_down.

    mr_tablecontrol->top_line = mr_tablecontrol->top_line + mv_looplines.
    IF mr_tablecontrol->top_line >= mv_linecount.
      mr_tablecontrol->top_line = mv_linecount - mv_looplines + 1.
      IF mr_tablecontrol->top_line < 1.
        mr_tablecontrol->top_line = 1.
      ENDIF.
    ENDIF.

    zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_line( 1 ).

  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_top.

    mr_tablecontrol->top_line = 1.
    zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_line( 1 ).

  ENDMETHOD.


  METHOD zif_uitb_page_scroller~scroll_page_up.
    mr_tablecontrol->top_line = mr_tablecontrol->top_line - mv_looplines.
    IF mr_tablecontrol->top_line < 1.
      mr_tablecontrol->top_line = 1.
    ENDIF.

    zcl_uitb_cursor=>get_cursor( if_reset = abap_false )->set_line( 1 ).
  ENDMETHOD.


  METHOD zif_uitb_table~add_line.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE,
                   <ls_line>  TYPE any.

    ASSIGN mr_table_data->* TO <lt_table>.
    ASSIGN mr_current_line->* TO <ls_line>.

    APPEND INITIAL LINE TO <lt_table>.
    rv_new_index = sy-tabix.
  ENDMETHOD.


  METHOD zif_uitb_table~delete_all.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    ASSIGN mr_table_data->* TO <lt_table>.

    CLEAR <lt_table>.
  ENDMETHOD.


  METHOD zif_uitb_table~delete_current_line.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    zif_uitb_table~determine_current_line( ).

    IF mv_current_line <= 0.
      RETURN.
    ENDIF.

    ASSIGN mr_table_data->* TO <lt_table>.

    DELETE <lt_table> INDEX mv_current_line.
  ENDMETHOD.


  METHOD zif_uitb_table~determine_current_line.
    GET CURSOR LINE mv_current_line.
    mv_current_line = mv_current_line +  mr_tablecontrol->top_line - 1.
  ENDMETHOD.


  METHOD zif_uitb_table~determine_line_count.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.
    ASSIGN mr_table_data->* TO <lt_table>.

    mr_tablecontrol->lines = lines( <lt_table> ).
  ENDMETHOD.


  METHOD zif_uitb_table~get_current_line_index.
    rv_index = mv_current_line.
  ENDMETHOD.


  METHOD zif_uitb_table~pbo.
    mv_looplines = sy-loopc.

    zif_uitb_table~determine_line_count( ).

    zif_uitb_table~update_screen_attributes( ).
  ENDMETHOD.


  METHOD zif_uitb_table~update_fields.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE,
                   <ls_line>  TYPE any.

    ASSIGN mr_table_data->* TO <lt_table>.
    ASSIGN mr_current_line->* TO <ls_line>.

    IF line_exists( <lt_table>[ mr_tablecontrol->current_line ] ).
      MODIFY <lt_table> FROM <ls_line> INDEX mr_tablecontrol->current_line.
    ENDIF.

  ENDMETHOD.


  METHOD zif_uitb_table~update_screen_attributes.
    DATA: ls_screen TYPE screen.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE,
                   <ls_line>  TYPE any.

    ASSIGN mr_table_data->* TO <lt_table>.

    " are there entries?
    IF <lt_table> IS INITIAL.
      LOOP AT SCREEN INTO ls_screen.
        ls_screen-active = 0.
        MODIFY screen FROM ls_screen.
      ENDLOOP.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD zif_uitb_table~get_table_data.
    rr_table_data_itab = mr_table_data.
  ENDMETHOD.

  METHOD is_empty.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    ASSIGN mr_table_data->* TO <lt_table>.

    result = xsdbool( <lt_table> IS INITIAL ).
  ENDMETHOD.

ENDCLASS.
