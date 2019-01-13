CLASS zcl_uitb_alv_column DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_alv_metadata
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_uitb_alv_metadata_util.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        ir_controller TYPE REF TO zif_uitb_alv_metadata_ctrller
        !is_data      TYPE lvc_s_fcat .
    METHODS get_tooltip
      RETURNING
        VALUE(result) TYPE lvc_tip.
    METHODS set_tooltip
      IMPORTING
        value TYPE lvc_tip.
    METHODS get_ddic_datatype
      RETURNING
        VALUE(result) TYPE datatype_d.
    METHODS set_ddic_datatype
      IMPORTING
        value TYPE datatype_d.
    METHODS get_ddic_inttype
      RETURNING
        VALUE(result) TYPE inttype.
    METHODS set_ddic_inttype
      IMPORTING
        value TYPE inttype.
    METHODS get_ddic_intlen
      RETURNING
        VALUE(result) TYPE intlen.
    METHODS set_ddic_intlen
      IMPORTING
        value TYPE intlen.
    METHODS get_ddic_decimals
      RETURNING
        VALUE(result) TYPE decimals.
    METHODS set_ddic_decimals
      IMPORTING
        value TYPE decimals.
    METHODS get_edit_mask
      RETURNING
        VALUE(result) TYPE lvc_edtmsk.
    METHODS set_edit_mask
      IMPORTING
        value TYPE lvc_edtmsk.
    METHODS is_lowercase
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS set_lowercase
      IMPORTING
        value TYPE abap_bool DEFAULT abap_true.
    METHODS is_optimized
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS set_optimized
      IMPORTING
        value TYPE abap_bool DEFAULT abap_true.
    METHODS set_ddic_reference
      IMPORTING
        iv_field TYPE fieldname
        iv_table TYPE tabname.
    METHODS has_leading_spaces
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS set_leading_spaces
      IMPORTING
        value TYPE abap_bool DEFAULT abap_true.
    METHODS is_symbol
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS set_symbol
      IMPORTING
        value TYPE abap_bool DEFAULT abap_true.
    METHODS set_short_description
      IMPORTING
        !value TYPE scrtext_s .
    METHODS set_icon
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true.
    METHODS get_name
      RETURNING
        VALUE(result) TYPE lvc_fname .
    METHODS set_visible
      IMPORTING
        !value TYPE abap_bool .
    METHODS set_technical
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true.
    METHODS set_descriptions
      IMPORTING
        !iv_short  TYPE scrtext_s DEFAULT space
        !iv_medium TYPE scrtext_m DEFAULT space
        !iv_long   TYPE scrtext_l DEFAULT space
        iv_tooltip TYPE lvc_tip OPTIONAL.
    METHODS set_quantity_column
      IMPORTING
        value TYPE lvc_qfname.
    METHODS set_currency_column
      IMPORTING
        value TYPE lvc_cfname.
    METHODS set_medium_description
      IMPORTING
        !value TYPE scrtext_m .
    METHODS set_color
      IMPORTING
        value TYPE lvc_emphsz.
    METHODS set_long_description
      IMPORTING
        !value TYPE scrtext_l .
    METHODS set_key
      IMPORTING
        value TYPE abap_bool DEFAULT abap_true.
    METHODS update
      IMPORTING
        !value TYPE lvc_s_fcat .
    METHODS set_hotspot
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true.
    METHODS set_style
      IMPORTING
        !value TYPE lvc_style .

    METHODS set_output_length
      IMPORTING
        !value TYPE lvc_outlen .
    METHODS to_structure
      RETURNING
        VALUE(result) TYPE lvc_s_fcat .
    METHODS set_editable
      IMPORTING
        value TYPE abap_bool DEFAULT abap_true.
    METHODS is_editable
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS get_cell_type
      RETURNING
        VALUE(result) TYPE i.
    METHODS set_cell_type
      IMPORTING
        value TYPE i DEFAULT zif_uitb_c_alv_cell_types=>text.
    METHODS is_f4
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS set_f4
      IMPORTING
        value TYPE abap_bool DEFAULT abap_true.
    METHODS has_custom_f4
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS set_custom_f4
      IMPORTING
        value TYPE abap_bool DEFAULT abap_true.
    METHODS get_dropdown_handle
      RETURNING
        VALUE(result) TYPE i.
    METHODS set_dropdown_handle
      IMPORTING
        value TYPE i.
    METHODS set_conversion_exit
      IMPORTING
        value TYPE convexit.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_data TYPE lvc_s_fcat .
    DATA mf_custom_f4 TYPE abap_bool.
    DATA mv_cell_type TYPE i.
    DATA mf_editable TYPE abap_bool.
    DATA mf_leading_spaces TYPE abap_bool.
ENDCLASS.



CLASS zcl_uitb_alv_column IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ir_controller = ir_controller ).
    ms_data = is_data.
  ENDMETHOD.


  METHOD get_cell_type.
    result = mv_cell_type.
  ENDMETHOD.


  METHOD get_ddic_datatype.
    result = ms_data-datatype.
  ENDMETHOD.


  METHOD get_ddic_decimals.
    result = ms_data-decimals.
  ENDMETHOD.


  METHOD get_ddic_intlen.
    result = ms_data-intlen.
  ENDMETHOD.


  METHOD get_ddic_inttype.
    result = ms_data-inttype.
  ENDMETHOD.


  METHOD get_dropdown_handle.
    result = ms_data-drdn_hndl.
  ENDMETHOD.


  METHOD get_edit_mask.
    result = ms_data-edit_mask.
  ENDMETHOD.


  METHOD get_name.
    result = ms_data-fieldname.
  ENDMETHOD.


  METHOD get_tooltip.
    result = ms_data-tooltip.
  ENDMETHOD.


  METHOD has_custom_f4.
    result = mf_custom_f4.
  ENDMETHOD.


  METHOD has_leading_spaces.
    result = mf_leading_spaces.
  ENDMETHOD.


  METHOD is_editable.
    result = mf_editable.
  ENDMETHOD.


  METHOD is_f4.
    result = ms_data-f4availabl.
  ENDMETHOD.


  METHOD is_lowercase.
  ENDMETHOD.


  METHOD is_optimized.
    result = ms_data-col_opt.
  ENDMETHOD.


  METHOD is_symbol.
    result = ms_data-symbol.
  ENDMETHOD.


  METHOD set_cell_type.
    mv_cell_type = value.

    IF mv_cell_type = zif_uitb_c_alv_cell_types=>checkbox OR
       mv_cell_type = zif_uitb_c_alv_cell_types=>checkbox_hotspot.
      set_f4( abap_false ).
      set_icon( abap_false ).
      set_symbol( abap_false ).
    ENDIF.

    set_setter_changed( iv_method = 'SET_CELL_TYPE' ).
  ENDMETHOD.


  METHOD set_color.
    ms_data-emphasize = value.

    set_setter_changed( iv_method = 'SET_COLOR' ).
  ENDMETHOD.


  METHOD set_conversion_exit.
    ms_data-convexit = value.
  ENDMETHOD.


  METHOD set_currency_column.
    ms_data-cfieldname = value.

    set_setter_changed( iv_method = 'SET_CURRENCY_COLUMN' ).
  ENDMETHOD.


  METHOD set_custom_f4.
    mf_custom_f4 = value.
  ENDMETHOD.


  METHOD set_ddic_datatype.
    ms_data-datatype = value.

    set_setter_changed( iv_method = 'SET_DDIC_DATATYPE' ).
  ENDMETHOD.


  METHOD set_ddic_decimals.
    ms_data-decimals = value.

    set_setter_changed( iv_method = 'SET_DDIC_DECIMALS' ).
  ENDMETHOD.


  METHOD set_ddic_intlen.
    ms_data-intlen = value.

    set_setter_changed( iv_method = 'SET_DDIC_INTLEN' ).
  ENDMETHOD.


  METHOD set_ddic_inttype.
    ms_data-inttype = value.

    set_setter_changed( iv_method = 'SET_DDIC_INTTYPE' ).
  ENDMETHOD.


  METHOD set_ddic_reference.
    " @TODO: check if ddic reference exists

    ms_data-ref_table = iv_table.
    ms_data-ref_field = iv_field.
  ENDMETHOD.


  METHOD set_descriptions.
    ms_data-scrtext_s = iv_short.
    ms_data-scrtext_m = iv_medium.
    ms_data-scrtext_l = iv_long.

    if iv_medium is supplied and iv_medium is not INITIAL.
      ms_data-reptext = ms_data-scrtext_m.
    ELSEif iv_long is supplied and iv_long is not initial.
      ms_data-reptext = ms_data-scrtext_l.
    ENDIF.

    IF iv_tooltip IS SUPPLIED.
      set_tooltip( iv_tooltip ).
    ENDIF.

    set_setter_changed( iv_method = 'SET_DESCRIPTIONS' ).
  ENDMETHOD.


  METHOD set_dropdown_handle.
    ms_data-drdn_hndl = value.
    ms_data-drdn_alias = abap_true.
  ENDMETHOD.


  METHOD set_editable.
    mf_editable = value.

    set_setter_changed( iv_method = 'SET_EDITABLE' ).
  ENDMETHOD.


  METHOD set_edit_mask.
    ms_data-edit_mask = value.

    set_setter_changed( iv_method = 'SET_EDIT_MASK' ).
  ENDMETHOD.


  METHOD set_f4.
    ms_data-f4availabl = value.

    set_setter_changed( iv_method = 'SET_F4' ).
  ENDMETHOD.


  METHOD set_hotspot.
    ms_data-hotspot = value.

    set_setter_changed( iv_method = 'SET_HOTSPOT' ).
  ENDMETHOD.


  METHOD set_icon.
    ms_data-icon = value.

    set_setter_changed( iv_method = 'SET_ICON' ).
  ENDMETHOD.


  METHOD set_key.
    ms_data-key = value.

    set_setter_changed( iv_method = 'SET_KEY' ).
  ENDMETHOD.


  METHOD set_leading_spaces.
    mf_leading_spaces = value.
  ENDMETHOD.


  METHOD set_long_description.
    ms_data-scrtext_l = value.

    set_setter_changed( iv_method = 'SET_LONG_DESCRIPTION' ).
  ENDMETHOD.


  METHOD set_lowercase.
    DATA(lv_old) = ms_data-lowercase.

    ms_data-lowercase = value.

    CHECK lv_old <> value.

    set_setter_changed( iv_method = 'SET_LOWERCASE' ).
  ENDMETHOD.


  METHOD set_medium_description.
    ms_data-scrtext_m = value.

    set_setter_changed( iv_method = 'SET_MEDIUM_DESCRIPTION' ).
  ENDMETHOD.


  METHOD set_optimized.
    ms_data-col_opt = value.
  ENDMETHOD.


  METHOD set_output_length.
    ms_data-outputlen = value.

    set_setter_changed( iv_method = 'SET_OUTPUT_LENGTH' ).
  ENDMETHOD.


  METHOD set_quantity_column.
    ms_data-qfieldname = value.

    set_setter_changed( iv_method = 'SET_QUANTITY_COLUMN' ).
  ENDMETHOD.


  METHOD set_short_description.
    ms_data-scrtext_s = value.

    set_setter_changed( iv_method = 'SET_SHORT_DESCRIPTION' ).
  ENDMETHOD.


  METHOD set_style.
    ms_data-style = value.

    set_setter_changed( iv_method = 'SET_STYLE' ).
  ENDMETHOD.


  METHOD set_symbol.
    ms_data-symbol = value.

    set_setter_changed( iv_method = 'SET_SYMBOL' ).
  ENDMETHOD.


  METHOD set_technical.
    ms_data-tech = value.

    set_setter_changed( iv_method = 'SET_TECHNICAL' ).
  ENDMETHOD.


  METHOD set_tooltip.
    ms_data-tooltip = value.

    set_setter_changed( iv_method = 'SET_TOOLTIP' ).
  ENDMETHOD.


  METHOD set_visible.
    ms_data-no_out = xsdbool( value = abap_false ).

    set_setter_changed( iv_method = 'SET_VISIBLE' ).
  ENDMETHOD.


  METHOD to_structure.
    result = ms_data.

*    result-reptext =
*    result-coltext = ms_data-scrtext_m.
    result-edit = mf_editable.

    result-parameter0 = mf_leading_spaces.

    CASE mv_cell_type.

      WHEN zif_uitb_c_alv_cell_types=>checkbox.
        result-checkbox = abap_true.

      WHEN zif_uitb_c_alv_cell_types=>checkbox_hotspot.
        result-checkbox = abap_true.
        result-hotspot = abap_true.

      WHEN zif_uitb_c_alv_cell_types=>dropdown.

      WHEN zif_uitb_c_alv_cell_types=>hotspot.
        result-hotspot = abap_true.

      WHEN zif_uitb_c_alv_cell_types=>button.
        result-style = cl_gui_alv_grid=>mc_style_button.

    ENDCASE.
  ENDMETHOD.


  METHOD update.
    ms_data = value.
  ENDMETHOD.
ENDCLASS.
