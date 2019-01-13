class ZCL_UITB_ALV_COLUMNS definition
  public
  inheriting from ZCL_UITB_ALV_METADATA
  final
  create public

  global friends ZCL_UITB_ALV_DATA_DESCR
                 ZCL_UITB_ALV_METADATA_UTIL .

public section.

  interfaces ZIF_UITB_LIST .

  types:
    BEGIN OF ty_alv_column,
        columnname TYPE lvc_fname,
        ref        TYPE REF TO zcl_uitb_alv_column,
      END OF ty_alv_column .
  types:
    tt_alv_columns TYPE STANDARD TABLE OF ty_alv_column WITH KEY columnname .

  methods ADD_COLUMN
    importing
      !IS_DATA type LVC_S_FCAT .
  methods CONSTRUCTOR
    importing
      !IR_DATA type ref to DATA
      !IR_CONTROLLER type ref to ZIF_UITB_ALV_METADATA_CTRLLER .
  methods CONVERT_TO_FIELDCAT
    returning
      value(RESULT) type LVC_T_FCAT .
  methods GET_COLOR_COLUMN
    returning
      value(RESULT) type LVC_FNAME .
  methods GET_COLOR_COLUMN_SIMPLE
    returning
      value(RESULT) type LVC_CIFNM .
  methods GET_COLUMN
    importing
      !IV_COLUMNNAME type LVC_FNAME
    returning
      value(RESULT) type ref to ZCL_UITB_ALV_COLUMN
    raising
      ZCX_UITB_ALV_NOT_FOUND .
    "! Returns the current column header mode
    "! @parameter result | current column header mode
  methods GET_COLUMN_HEADER_MODE
    returning
      value(RESULT) type I .
  methods GET_DESCRIPTION_LANGUAGE
    returning
      value(RESULT) type LANGU .
  methods GET_STYLE_COLUMN
    returning
      value(RESULT) type LVC_FNAME .
  methods IS_COLUMN_HEADERS_VISIBLE
    returning
      value(RESULT) type ABAP_BOOL .
  methods IS_KEY_FIXATION
    returning
      value(RESULT) type ABAP_BOOL .
  methods IS_OPTIMIZED
    returning
      value(RESULT) type ABAP_BOOL .
  methods SET_COLOR_COLUMN
    importing
      !VALUE type LVC_FNAME .
  methods SET_COLOR_COLUMN_SIMPLE
    importing
      !VALUE type LVC_CIFNM .
  methods SET_COLUMN_HEADERS_VISIBLE
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE .
  methods SET_COLUMN_HEADER_MODE
    importing
      !IV_COLUMN_HEADER_MODE type I default ZIF_UITB_C_ALV_COLHEADER_MODE=>DEFAULT .
  methods SET_COLUMN_POSITION
    importing
      !IV_COLUMNNAME type LVC_FNAME
      !IV_POSITION type I
    returning
      value(RV_POS) type I .
  methods SET_DESCRIPTION_LANGUAGE
    importing
      !IV_LANGUAGE type LANGU default SY-LANGU .
  methods SET_EDITABLE
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE .
  methods SET_KEY_FIXATION
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE .
  methods SET_OPTIMIZED
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE .
  methods SET_SINGLE_CLICK_SORT
    importing
      !VALUE type ABAP_BOOL default ABAP_TRUE .
  methods SET_STYLE_COLUMN
    importing
      !VALUE type LVC_FNAME .
  methods UPDATE_COLUMN_DATA
    importing
      !IV_COLUMNNAME type LVC_FNAME
      !IS_DATA type LVC_S_FCAT .
  PROTECTED SECTION.
PRIVATE SECTION.

  DATA mt_columns TYPE tt_alv_columns .
  DATA mv_color_column_simple TYPE lvc_cifnm .
  DATA mv_style_column TYPE lvc_fname .
  DATA mv_color_column TYPE lvc_fname .
  DATA mf_optimized TYPE abap_bool .
  DATA mf_key_fixation TYPE abap_bool .
  DATA mv_col_header_mode TYPE i VALUE zif_uitb_c_alv_colheader_mode=>default ##NO_TEXT.
  DATA mv_language TYPE langu .
  DATA mr_table_structure TYPE REF TO data .
  DATA mf_single_click_sort TYPE abap_bool .
  DATA mf_col_header_visible TYPE abap_bool VALUE abap_true ##NO_TEXT.
ENDCLASS.



CLASS ZCL_UITB_ALV_COLUMNS IMPLEMENTATION.


  METHOD add_column.
    APPEND VALUE #( columnname = is_data-fieldname
                    ref        = NEW zcl_uitb_alv_column(
                                   is_data = is_data
                                   ir_controller = mr_controller
                                 ) ) TO mt_columns.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ir_controller = ir_controller
                        iv_name       = 'COLUMNS' ).
  ENDMETHOD.


  METHOD convert_to_fieldcat.
    result = VALUE #( FOR <ls_column> IN mt_columns ( <ls_column>-ref->to_structure( ) ) ).
  ENDMETHOD.


  METHOD get_color_column.
    result = mv_color_column.
  ENDMETHOD.


  METHOD get_color_column_simple.
    result = mv_color_column_simple.
  ENDMETHOD.


  METHOD get_column.
    TRY.
        result = mt_columns[ columnname = iv_columnname ]-ref.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_uitb_alv_not_found
          EXPORTING
            textid = zcx_uitb_alv_not_found=>zcx_uitb_alv_not_found
            msgv1  = 'COLUMN'
            msgv2  = |{ iv_columnname }|
            msgv3  = 'ZCL_UITB_ALV_COLUMNS'
            msgv4  = 'GET_COLUMN'.
    ENDTRY.
  ENDMETHOD.


  METHOD get_column_header_mode.
    result = mv_col_header_mode.
  ENDMETHOD.


  METHOD get_description_language.
    result = cond #( when mv_language is initial then sy-langu else mv_language ).
  ENDMETHOD.


  METHOD get_style_column.
    result = mv_style_column.
  ENDMETHOD.


  METHOD is_column_headers_visible.
    result = mf_col_header_visible.
  ENDMETHOD.


  METHOD is_key_fixation.
    result = mf_key_fixation.
  ENDMETHOD.


  METHOD is_optimized.
    result = mf_optimized.
  ENDMETHOD.


  METHOD set_color_column.
    FIELD-SYMBOLS: <ls_data>  TYPE any,
                   <lv_field> TYPE any.

    ASSIGN mr_table_structure->* TO <ls_data>.
    CHECK sy-subrc = 0.

    ASSIGN COMPONENT value OF STRUCTURE <ls_data> TO <lv_field>.
    IF sy-subrc <> 0.
      RETURN.
*... TODO: raise appropriate exception
*      raise_column_not_in_data_table(
*        method     = 'SET_COLOR_COLUMN'
*        columnname = value ).
    ENDIF.

    DATA(lr_type) = cl_abap_typedescr=>describe_by_data( <lv_field> ).

    IF lr_type->absolute_name <> '\TYPE=LVC_T_SCOL'.
      RETURN.
*... TODO: raise appropriate exception
*      raise_invalid_input_type(
*        method     = 'SET_COLOR_COLUMN'
*        columnname = value
*        type       = 'LVC_T_SCOL' ).
    ENDIF.

    mv_color_column = value.

*... if the field exists as a column, set it to technical
    TRY.
        DATA(lr_column) = get_column( value ).
        lr_column->set_technical( ).
      CATCH zcx_uitb_alv_not_found.
    ENDTRY.

    set_setter_changed( iv_method = 'SET_COLOR_COLUMN' ).
  ENDMETHOD.


  METHOD set_color_column_simple.
    mv_color_column_simple = value.

    DATA(lr_column) = get_column( value ).
    lr_column->set_technical( ).

    set_setter_changed( iv_method = 'SET_COLOR_COLUMN_SIMPLE' ).
  ENDMETHOD.


  METHOD set_column_headers_visible.
    mf_col_header_visible = value.
  ENDMETHOD.


  METHOD set_column_header_mode.
    mv_col_header_mode = iv_column_header_mode.
  ENDMETHOD.


  METHOD set_column_position.
    CHECK iv_columnname IS NOT INITIAL.

    DATA(lv_col_index) = line_index( mt_columns[ columnname = iv_columnname ] ).
    IF lv_col_index <> 0.
      DATA(ls_column) = mt_columns[ lv_col_index ].
      DELETE mt_columns INDEX lv_col_index.
    ELSE.
      EXIT.
    ENDIF.

    IF iv_position IS INITIAL OR iv_position GT lines( mt_columns ).
      APPEND ls_column TO mt_columns.
      rv_pos = sy-tabix.
    ELSE.
      INSERT ls_column INTO mt_columns INDEX iv_position.
      rv_pos = iv_position.
    ENDIF.

    set_setter_changed( iv_method = 'SET_COLUMN_POSITION' ).
  ENDMETHOD.


  METHOD set_description_language.
    mv_language = iv_language.
  ENDMETHOD.


  METHOD set_editable.
    LOOP AT mt_columns ASSIGNING FIELD-SYMBOL(<ls_column>).
      <ls_column>-ref->set_editable( value ).
    ENDLOOP.
  ENDMETHOD.


  METHOD set_key_fixation.
    mf_key_fixation = value.

    set_setter_changed( iv_method = 'SET_KEY_FIXATION' ).
  ENDMETHOD.


  METHOD set_optimized.
    mf_optimized = value.

    set_setter_changed( iv_method = 'SET_OPTIMIZED' ).
  ENDMETHOD.


  METHOD set_single_click_sort.
    mf_single_click_sort = value.

    set_setter_changed( iv_method = 'SET_SINGLE_CLICK_SORT' ).
  ENDMETHOD.


  METHOD set_style_column.

    FIELD-SYMBOLS: <ls_data>  TYPE any,
                   <lv_field> TYPE any.

    ASSIGN mr_table_structure->* TO <ls_data>.
    CHECK sy-subrc = 0.

    ASSIGN COMPONENT value OF STRUCTURE <ls_data> TO <lv_field>.
    IF sy-subrc <> 0.
      RETURN.
*... TODO: raise appropriate exception
*      raise_column_not_in_data_table(
*        method     = 'SET_COLOR_COLUMN'
*        columnname = value ).
    ENDIF.

    DATA(lr_type) = cl_abap_typedescr=>describe_by_data( <lv_field> ).

    IF lr_type->absolute_name <> '\TYPE=LVC_T_STYL'.
      RETURN.
*... TODO: raise appropriate exception
*      raise_invalid_input_type(
*        method     = 'SET_COLOR_COLUMN'
*        columnname = value
*        type       = 'LVC_T_SCOL' ).
    ENDIF.

    mv_style_column = value.

*... if the field exists as a column, set it to technical
    TRY.
        DATA(lr_column) = get_column( value ).
        lr_column->set_technical( ).
      CATCH zcx_uitb_alv_not_found.
    ENDTRY.

    set_setter_changed( iv_method = 'SET_STYLE_COLUMN' ).
  ENDMETHOD.


  METHOD update_column_data.
    DATA(ls_fcat) = is_data.

    CLEAR ls_fcat-style.

    CAST zcl_uitb_alv_column( mt_columns[ columnname = iv_columnname ]-ref )->update( is_data ).
  ENDMETHOD.


  METHOD zif_uitb_list~add.
  ENDMETHOD.


  METHOD zif_uitb_list~clear.

    CLEAR:
       mt_columns,
       mv_color_column_simple,
       mv_style_column,
       mv_color_column,
       mf_optimized,
       mf_key_fixation,
       mv_col_header_mode,
       mv_language,
       mr_table_structure.

  ENDMETHOD.


  METHOD zif_uitb_list~get_element.
    TRY.
        rr_element = mt_columns[ iv_index ]-ref.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_uitb_element_not_found
          EXPORTING
            textid = zcx_uitb_element_not_found=>index_access
            index  = iv_index.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_uitb_list~get_iterator.
    rr_iterator = zcl_uitb_list_iterator=>create( ir_list = me ).
  ENDMETHOD.


  METHOD zif_uitb_list~remove.
  ENDMETHOD.


  METHOD zif_uitb_list~remove_at.
  ENDMETHOD.


  METHOD zif_uitb_list~size.
    rv_size = lines( mt_columns ).
  ENDMETHOD.
ENDCLASS.
