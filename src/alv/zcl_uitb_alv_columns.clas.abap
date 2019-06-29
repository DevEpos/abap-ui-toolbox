"! <p class="shorttext synchronized" lang="en">Columns of ALV</p>
CLASS zcl_uitb_alv_columns DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_alv_metadata
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_uitb_alv_data_descr
                 zcl_uitb_alv_metadata_util .

  PUBLIC SECTION.

    INTERFACES zif_uitb_list .

    TYPES:
      BEGIN OF ty_alv_column,
        columnname TYPE lvc_fname,
        ref        TYPE REF TO zcl_uitb_alv_column,
      END OF ty_alv_column .
    TYPES:
      tt_alv_columns TYPE STANDARD TABLE OF ty_alv_column WITH KEY columnname .

    METHODS add_column
      IMPORTING
        !is_data TYPE lvc_s_fcat .
    METHODS constructor
      IMPORTING
        !ir_data       TYPE REF TO data
        !ir_controller TYPE REF TO zif_uitb_alv_metadata_ctrller .
    METHODS convert_to_fieldcat
      RETURNING
        VALUE(result) TYPE lvc_t_fcat .
    "! <p class="shorttext synchronized" lang="en">Gets the name of the color column</p>
    METHODS get_color_column
      RETURNING
        VALUE(result) TYPE lvc_fname .
    METHODS get_color_column_simple
      RETURNING
        VALUE(result) TYPE lvc_cifnm .
    METHODS get_column
      IMPORTING
        !iv_columnname TYPE lvc_fname
      RETURNING
        VALUE(result)  TYPE REF TO zcl_uitb_alv_column
      RAISING
        zcx_uitb_alv_not_found .
    "! Returns the current column header mode
    "! @parameter result | current column header mode
    METHODS get_column_header_mode
      RETURNING
        VALUE(result) TYPE i .
    METHODS get_description_language
      RETURNING
        VALUE(result) TYPE langu .
    METHODS get_style_column
      RETURNING
        VALUE(result) TYPE lvc_fname .
    "! <p class="shorttext synchronized" lang="en">Is column headers visible</p>
    METHODS is_column_headers_visible
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS is_key_fixation
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Get Column Optimization</p>
    METHODS is_optimized
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Sets the color column</p>
    METHODS set_color_column
      IMPORTING
        !value TYPE lvc_fname .
    METHODS set_color_column_simple
      IMPORTING
        !value TYPE lvc_cifnm .
    "! <p class="shorttext synchronized" lang="en">Set column headers visible</p>
    METHODS set_column_headers_visible
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true .
    METHODS set_column_header_mode
      IMPORTING
        !iv_column_header_mode TYPE i DEFAULT zif_uitb_c_alv_colheader_mode=>default .
    METHODS get_column_position
      IMPORTING
        iv_columnname      TYPE lvc_fname
      RETURNING
        VALUE(rv_position) TYPE i.
    METHODS set_column_position
      IMPORTING
        !iv_columnname TYPE lvc_fname
        !iv_position   TYPE i
      RETURNING
        VALUE(rv_pos)  TYPE i .
    "! Moves the given column after <strong>iv_after_column</strong>
    METHODS move_column_after
      IMPORTING
        iv_column       TYPE lvc_fname
        iv_after_column TYPE lvc_fname
      RETURNING
        VALUE(rv_pos)   TYPE i .
    "! Moves the given column before <strong>iv_before_column</strong>
    METHODS move_column_before
      IMPORTING
        iv_column        TYPE lvc_fname
        iv_before_column TYPE lvc_fname
      RETURNING
        VALUE(rv_pos)    TYPE i .
    METHODS set_description_language
      IMPORTING
        !iv_language TYPE langu DEFAULT sy-langu .
    METHODS set_editable
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true .
    METHODS set_key_fixation
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true .
    "! <p class="shorttext synchronized" lang="en">Activate Column optimization</p>
    METHODS set_optimized
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true .
    "! <p class="shorttext synchronized" lang="en">Set column to single click sort mode</p>
    METHODS set_single_click_sort
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true .
    METHODS set_style_column
      IMPORTING
        !value TYPE lvc_fname .
    METHODS update_column_data
      IMPORTING
        !iv_columnname TYPE lvc_fname
        !is_data       TYPE lvc_s_fcat .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_columns TYPE tt_alv_columns .
    DATA mv_color_column_simple TYPE lvc_cifnm .
    DATA mv_style_column TYPE lvc_fname .
    "! <p class="shorttext synchronized" lang="en">ALV control: Field name of internal table field</p>
    DATA mv_color_column TYPE lvc_fname .
    DATA mf_optimized TYPE abap_bool .
    DATA mf_key_fixation TYPE abap_bool .
    DATA mv_col_header_mode TYPE i VALUE zif_uitb_c_alv_colheader_mode=>default ##NO_TEXT.
    DATA mv_language TYPE langu .
    DATA mr_table_structure TYPE REF TO data .
    DATA mf_single_click_sort TYPE abap_bool .
    DATA mf_col_header_visible TYPE abap_bool VALUE abap_true ##NO_TEXT.
ENDCLASS.



CLASS zcl_uitb_alv_columns IMPLEMENTATION.


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
    result = COND #( WHEN mv_language IS INITIAL THEN sy-langu ELSE mv_language ).
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

  METHOD get_column_position.
    rv_position = line_index( mt_columns[ columnname = iv_columnname ] ).
  ENDMETHOD.

  METHOD move_column_after.
    DATA(lv_target_column_index) = line_index( mt_columns[ columnname = iv_after_column ] ).
    IF lv_target_column_index <> 0.
      rv_pos = set_column_position(
        iv_columnname = iv_column
        iv_position   = lv_target_column_index + 1
      ).
    ENDIF.
    set_setter_changed( iv_method = 'MOVE_COLUMN_AFTER' ).
  ENDMETHOD.

  METHOD move_column_before.
    DATA(lv_target_column_index) = line_index( mt_columns[ columnname = iv_before_column ] ).
    IF lv_target_column_index <> 0.
      rv_pos = set_column_position(
        iv_columnname = iv_column
        iv_position   = lv_target_column_index
      ).
    ENDIF.

    set_setter_changed( iv_method = 'MOVE_COLUMN_BEFORE' ).
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
