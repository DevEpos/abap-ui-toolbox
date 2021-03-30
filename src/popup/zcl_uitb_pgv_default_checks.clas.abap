"! <p class="shorttext synchronized" lang="en">Perform default field validation checks</p>
CLASS zcl_uitb_pgv_default_checks DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates new instance</p>
      constructor
        IMPORTING
          io_evt_fields TYPE REF TO zif_uitb_pgv_check_evt_fields
          it_fields     TYPE zif_uitb_pgv_popup=>tt_dialog_field_ext,
      "! <p class="shorttext synchronized" lang="en">Checks field values</p>
      check_fields.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      ty_fld_check_range TYPE RANGE OF spo_value.

    DATA:
      mo_evt_fields TYPE REF TO zif_uitb_pgv_check_evt_fields,
      mt_fields     TYPE zif_uitb_pgv_popup=>tt_dialog_field_ext.

    METHODS:
      set_validation_error
        IMPORTING
          iv_fieldname TYPE sval-fieldname
          iv_tabname   TYPE sval-tabname
          iv_label     TYPE scrtext_m
          is_check     TYPE zif_uitb_pgv_popup=>ty_validation_config
          iv_value     TYPE string,
      get_error_text
        IMPORTING
          is_check             TYPE zif_uitb_pgv_popup=>ty_validation_config
          iv_field_label       TYPE string
        RETURNING
          VALUE(rv_error_text) TYPE string,
      get_field_label
        IMPORTING
          iv_tabname      TYPE sval-tabname
          iv_fieldname    TYPE sval-fieldname
          iv_label        TYPE scrtext_m
        RETURNING
          VALUE(rv_label) TYPE string.
ENDCLASS.


CLASS zcl_uitb_pgv_default_checks IMPLEMENTATION.

  METHOD constructor.
    mo_evt_fields = io_evt_fields.
    mt_fields = it_fields.
  ENDMETHOD.

  METHOD check_fields.
    DATA: lt_check_range TYPE ty_fld_check_range.

    APPEND INITIAL LINE TO lt_check_range ASSIGNING FIELD-SYMBOL(<ls_check_range>).
    <ls_check_range>-sign = 'I'.

    LOOP AT mo_evt_fields->get_fields( ) ASSIGNING FIELD-SYMBOL(<ls_input_field>).
      ASSIGN mt_fields[
        fieldname = <ls_input_field>-fieldname
        tabname   = <ls_input_field>-tabname ] TO FIELD-SYMBOL(<ls_field_config>).

      CHECK:
        sy-subrc = 0,
        <ls_field_config>-validation IS NOT INITIAL.

      <ls_check_range>-option = <ls_field_config>-validation-comparator.
      <ls_check_range>-low = <ls_field_config>-validation-value_low.
      <ls_check_range>-high = <ls_field_config>-validation-value_high.

      DATA(lv_value) = condense( <ls_input_field>-value ).

      IF lv_value NOT IN lt_check_range.
        set_validation_error(
          iv_fieldname = <ls_input_field>-fieldname
          iv_tabname   = <ls_input_field>-tabname
          iv_label     = <ls_input_field>-fieldtext
          is_check     = <ls_field_config>-validation
          iv_value     = lv_value ).
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_validation_error.
    DATA(lv_field_label) = get_field_label(
      iv_tabname   = iv_tabname
      iv_fieldname = iv_fieldname
      iv_label     = iv_label ).
    DATA(lv_error_text) = get_error_text(
      is_check       = is_check
      iv_field_label = lv_field_label ).

    zcl_uitb_appl_util=>split_string_for_message(
      EXPORTING iv_string = lv_error_text
      IMPORTING ev_msgv1  = DATA(lv_msgv1)
                ev_msgv2  = DATA(lv_msgv2)
                ev_msgv3  = DATA(lv_msgv3)
                ev_msgv4  = DATA(lv_msgv4) ).

    mo_evt_fields->set_validation_error( VALUE #(
      errorfield = iv_fieldname
      errortab   = iv_tabname
      msgid      = '00'
      msgty      = 'E'
      msgno      = '001'
      msgv1      = lv_msgv1
      msgv2      = lv_msgv2
      msgv3      = lv_msgv3
      msgv4      = lv_msgv4 ) ).
  ENDMETHOD.


  METHOD get_error_text.
    CASE is_check-comparator.

      WHEN 'EQ'.
        rv_error_text = |{ iv_field_label } is not equal to { is_check-value_low }|.

      WHEN 'NE'.
        rv_error_text = |{ iv_field_label } is equal to { is_check-value_low }|.

      WHEN 'GE'.
        rv_error_text = |{ iv_field_label } must be greater or equal to { is_check-value_low }|.

      WHEN 'GT'.
        rv_error_text = |{ iv_field_label } must be greater than { is_check-value_low }|.

      WHEN 'LE'.
        rv_error_text = |{ iv_field_label } must be lesser or equal to { is_check-value_low }|.

      WHEN 'LT'.
        rv_error_text = |{ iv_field_label } must be lesser than { is_check-value_low }|.

      WHEN 'CP'.
        rv_error_text = |{ iv_field_label } does not match the pattern { is_check-value_low }|.

      WHEN 'NP'.
        rv_error_text = |{ iv_field_label } matches the pattern { is_check-value_low }|.

      WHEN 'BT'.
        rv_error_text = |{ iv_field_label } is not between { is_check-value_low } and { is_check-value_high }|.

      WHEN 'NB'.
        rv_error_text = |{ iv_field_label } is between { is_check-value_low } and { is_check-value_high }|.

    ENDCASE.
  ENDMETHOD.


  METHOD get_field_label.
    IF iv_label IS NOT INITIAL.
      rv_label = iv_label.
    ELSE.
      DATA(lo_field_descr) = cl_abap_typedescr=>describe_by_name( |{ iv_tabname }-{ iv_fieldname }| ).
      IF lo_field_descr->kind = cl_abap_typedescr=>kind_elem.
        DATA(lo_elem_descr) = CAST cl_abap_elemdescr( lo_field_descr ).
        DATA(ls_ddic_field) = lo_elem_descr->get_ddic_field( ).

        IF ls_ddic_field-scrtext_m IS NOT INITIAL.
          rv_label = ls_ddic_field-scrtext_m.
        ELSEIF ls_ddic_field-scrtext_s IS NOT INITIAL.
          rv_label = ls_ddic_field-scrtext_s.
        ELSEIF ls_ddic_field-scrtext_l IS NOT INITIAL.
          rv_label = ls_ddic_field-scrtext_l.
        ELSE.
          rv_label = iv_fieldname.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
