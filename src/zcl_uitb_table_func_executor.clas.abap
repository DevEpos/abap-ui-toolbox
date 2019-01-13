CLASS zcl_uitb_table_func_executor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_value_udpate,
        compname TYPE fieldname,
        value    TYPE REF TO data,
      END OF ty_value_udpate .
    TYPES:
      tt_value_udpate TYPE STANDARD TABLE OF ty_value_udpate WITH KEY compname .

    METHODS constructor
      IMPORTING
        !ir_table TYPE REF TO data .
    METHODS max_value
      IMPORTING
        !iv_field     TYPE string
      RETURNING
        VALUE(result) TYPE sy-tabix .
    METHODS update_values
      IMPORTING
        !it_values TYPE zcl_uitb_table_func_executor=>tt_value_udpate
        !iv_where  TYPE string OPTIONAL .
    METHODS max_length
      IMPORTING
        !iv_comp_name        TYPE string
      RETURNING
        VALUE(rv_max_length) TYPE i .
    METHODS search
      IMPORTING
        !iv_search_value  TYPE string
        !it_search_fields TYPE string_table
        !iv_start_index   TYPE sy-tabix
      RETURNING
        VALUE(rv_index)   TYPE sy-tabix .
    METHODS count_lines
      IMPORTING
        !it_field_selopts TYPE zuitb_field_range_map_itab
        !if_use_or        TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rv_count)   TYPE i .
    METHODS exists
      IMPORTING
        !it_field_selopts TYPE zuitb_field_range_map_itab
        !if_use_or        TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result)     TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mr_table TYPE REF TO data .

    METHODS create_dynamic_where
      IMPORTING
        !it_field_selopts TYPE zuitb_field_range_map_itab
        !if_use_or        TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rt_where)   TYPE string_table .
    METHODS search_internal
      IMPORTING
        !iv_search_value  TYPE string
        !it_search_fields TYPE string_table
      CHANGING
        !cs_current_line  TYPE any
      RETURNING
        VALUE(rf_found)   TYPE boolean .
ENDCLASS.



CLASS zcl_uitb_table_func_executor IMPLEMENTATION.


  METHOD constructor.
    mr_table = ir_table.
  ENDMETHOD.


  METHOD count_lines.
    FIELD-SYMBOLS: <lt_table> TYPE table.

    ASSIGN mr_table->* TO <lt_table>.

    DATA(lt_where) = create_dynamic_where(
      it_field_selopts = it_field_selopts
      if_use_or        = if_use_or
    ).

    TRY.
        " perform counting operation
        LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_line>) WHERE (lt_where).
          ADD 1 TO rv_count.
        ENDLOOP.
      CATCH cx_sy_itab_dyn_loop.
        CLEAR rv_count.
    ENDTRY.

  ENDMETHOD.


  METHOD create_dynamic_where.
    DATA: lt_field_ranges  TYPE rsds_frange_t,
          lt_where_clauses TYPE rsds_twhere.

    " build dynamic where clause
    LOOP AT it_field_selopts ASSIGNING FIELD-SYMBOL(<ls_field_selopt>).
      APPEND VALUE rsds_frange(
          fieldname = <ls_field_selopt>-fieldname
          selopt_t  = VALUE #( FOR selopt IN <ls_field_selopt>-selopt_itab
                               ( sign   = selopt-sign
                                 option = selopt-option
                                 low    = selopt-low
                                 high   = selopt-high ) )
      ) TO lt_field_ranges.
    ENDLOOP.

    CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
      EXPORTING
        field_ranges  = VALUE rsds_trange( ( frange_t = lt_field_ranges ) )
      IMPORTING
        where_clauses = lt_where_clauses.

    LOOP AT lt_where_clauses ASSIGNING FIELD-SYMBOL(<ls_where_clause>).
      DATA(lv_index) = sy-tabix.
      APPEND LINES OF <ls_where_clause>-where_tab TO rt_where.
      IF lv_index <> lines( lt_where_clauses ).
        rt_where = VALUE #( BASE rt_where
                           ( COND string( WHEN if_use_or = abap_true THEN ` OR ` ELSE ` AND ` ) ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD exists.
    FIELD-SYMBOLS: <lt_table> TYPE table.

    ASSIGN mr_table->* TO <lt_table>.

    CLEAR result.

    DATA(lt_where) = create_dynamic_where(
      it_field_selopts = it_field_selopts
      if_use_or        = if_use_or
    ).

    TRY.
        " perform counting operation
        LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_line>) WHERE (lt_where).
          result = abap_true.
          RETURN.
        ENDLOOP.
      CATCH cx_sy_itab_dyn_loop.
        CLEAR result.
    ENDTRY.
  ENDMETHOD.


  METHOD max_length.
    FIELD-SYMBOLS: <lt_data> TYPE ANY TABLE.

    ASSIGN mr_table->* TO <lt_data>.

    LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).
      ASSIGN COMPONENT iv_comp_name OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_component>).
      IF sy-subrc = 0.
        DATA(lv_count) = strlen( <lv_component> ).
      ENDIF.

      IF rv_max_length < lv_count.
        rv_max_length = lv_count.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD max_value.
    DATA: lv_current_value TYPE sy-tabix.

    FIELD-SYMBOLS: <lt_data> TYPE ANY TABLE.

    ASSIGN mr_table->* TO <lt_data>.

    LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).
      ASSIGN COMPONENT iv_field OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_component>).
      IF sy-subrc = 0.
        lv_current_value = <lv_component>.
      ENDIF.

      IF result < lv_current_value.
        result = lv_current_value.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD search.
    DATA: lv_start_index TYPE sy-tabix.

    FIELD-SYMBOLS: <lt_table> TYPE table.

    IF mr_table IS INITIAL.
      rv_index = 0.
      RETURN.
    ENDIF.

    ASSIGN mr_table->* TO <lt_table>.

    " set the starting index
    lv_start_index = iv_start_index.
    IF lv_start_index < 0.
      lv_start_index = 1.
    ENDIF.

    DATA(lv_search_value) = to_upper( iv_search_value ).

    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_line>) FROM lv_start_index.
      IF search_internal( EXPORTING iv_search_value  = lv_search_value
                                    it_search_fields = it_search_fields
                          CHANGING  cs_current_line  = <ls_line> ).
        rv_index = sy-tabix.
        RETURN.
      ENDIF.
    ENDLOOP.

    CLEAR rv_index.

  ENDMETHOD.


  METHOD search_internal.
    " build table of reference values
    LOOP AT it_search_fields ASSIGNING FIELD-SYMBOL(<lv_field>).
      ASSIGN COMPONENT <lv_field> OF STRUCTURE cs_current_line TO FIELD-SYMBOL(<lv_search_field_val>).

      IF sy-subrc = 0.
        DATA(lv_value) = to_upper( <lv_search_field_val> ).
        IF lv_value = iv_search_value OR
          lv_value CS iv_search_value OR
          lv_value CP iv_search_value.
          rf_found = abap_true.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD update_values.
    FIELD-SYMBOLS: <lt_data> TYPE table.
    ASSIGN mr_table->* TO <lt_data>.

    LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>) WHERE (iv_where).
      LOOP AT it_values ASSIGNING FIELD-SYMBOL(<ls_new_value>).
        ASSIGN COMPONENT <ls_new_value>-compname OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_comp_value>).
        CHECK sy-subrc = 0.

        ASSIGN <ls_new_value>-value->* TO FIELD-SYMBOL(<lv_new_value>).
        IF sy-subrc = 0.
          " @TODO: catch wrong type error
          <lv_comp_value> = <lv_new_value>.
        ELSE.
          CLEAR <lv_comp_value>.
        ENDIF.

      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
