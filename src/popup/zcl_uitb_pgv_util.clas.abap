"! <p class="shorttext synchronized" lang="en">Utility for popup_get_values dialogs</p>
CLASS zcl_uitb_pgv_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES tt_input_val TYPE STANDARD TABLE OF sval  WITH EMPTY KEY .

    "! <p class="shorttext synchronized" lang="en">Popup with single input fields</p>
    CLASS-METHODS popup_get_value
      IMPORTING
        is_field      TYPE sval
        iv_title      TYPE string OPTIONAL
      EXPORTING
        ef_cancelled  TYPE abap_bool
      RETURNING
        VALUE(result) TYPE spo_value.
    "! <p class="shorttext synchronized" lang="en">Popup with multiple input fields</p>
    CLASS-METHODS popup_get_values
      IMPORTING
        !iv_title         TYPE string OPTIONAL
      CHANGING
        !ct_fields        TYPE tt_input_val
      RETURNING
        VALUE(rf_success) TYPE abap_bool .

    "! <p class="shorttext synchronized" lang="en">Popup with multiple input fields + check</p>
    CLASS-METHODS popup_get_values_w_check
      IMPORTING
        !iv_title         TYPE string OPTIONAL
      CHANGING
        !ct_fields        TYPE tt_input_val
      RETURNING
        VALUE(rf_success) TYPE abap_bool .

    "! <p class="shorttext synchronized" lang="en">Popup with single input field + check</p>
    CLASS-METHODS popup_get_value_w_check
      IMPORTING
        is_field     TYPE sval
        iv_title     TYPE string OPTIONAL
      EXPORTING
        ef_cancelled TYPE abap_bool
        ev_result    TYPE spo_value.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_uitb_pgv_util IMPLEMENTATION.

  METHOD popup_get_values.
    DATA: lv_rcode(1).

    rf_success = abap_true.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title  = iv_title
        start_column = '12'
        start_row    = '5'
      IMPORTING
        returncode   = lv_rcode
      TABLES
        fields       = ct_fields
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc <> 0 OR lv_rcode = 'A'.
      rf_success = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD popup_get_value.
    DATA: lv_rcode(1),
          lt_fields TYPE TABLE OF sval.

    result = is_field-value.

    lt_fields = VALUE #( ( is_field ) ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title  = iv_title
        start_column = '12'
        start_row    = '5'
      IMPORTING
        returncode   = lv_rcode
      TABLES
        fields       = lt_fields
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc <> 0 OR lv_rcode = 'A'.
      ef_cancelled = abap_true.
    ELSE.
      result = CONV string( lt_fields[ 1 ]-value ).
    ENDIF.

  ENDMETHOD.

  METHOD popup_get_values_w_check.
    DATA: lv_rcode(1).

    rf_success = abap_true.

    CALL FUNCTION 'POPUP_GET_VALUES_USER_CHECKED'
      EXPORTING
        popup_title  = iv_title
        start_column = '12'
        start_row    = '5'
        programname  = 'ZUITB_PGV_EXITS'
        formname     = 'EXIT_CHECK_FIELDS'
      IMPORTING
        returncode   = lv_rcode
      TABLES
        fields       = ct_fields
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc <> 0 OR lv_rcode = 'A'.
      rf_success = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD popup_get_value_w_check.
    DATA: lv_rcode(1),
          lt_fields TYPE TABLE OF sval.

    ev_result = is_field-value.

    lt_fields = VALUE #( ( is_field ) ).

    CALL FUNCTION 'POPUP_GET_VALUES_USER_CHECKED'
      EXPORTING
        popup_title  = iv_title
        start_column = '12'
        start_row    = '5'
        programname  = 'ZUITB_PGV_EXITS'
        formname     = 'EXIT_CHECK_FIELDS'
      IMPORTING
        returncode   = lv_rcode
      TABLES
        fields       = lt_fields.

    IF sy-subrc <> 0 OR lv_rcode = 'A'.
      ef_cancelled = abap_true.
    ELSE.
      ev_result = CONV string( lt_fields[ 1 ]-value ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
