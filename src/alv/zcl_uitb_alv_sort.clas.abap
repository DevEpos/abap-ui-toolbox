CLASS ZCL_UITB_ALV_SORT DEFINITION
  PUBLIC
  INHERITING FROM ZCL_UITB_alv_metadata
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS ZCL_UITB_alv_metadata_util.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        ir_column     TYPE REF TO ZCL_UITB_alv_column
        ir_controller TYPE REF TO ZIF_UITB_alv_metadata_ctrller OPTIONAL
        iv_sequence   TYPE i DEFAULT zif_uitb_c_alv_sorting=>ascending
        if_subtotal   TYPE abap_bool OPTIONAL.
    METHODS get_column_name
      RETURNING
        VALUE(result) TYPE lvc_fname .
    METHODS get_sequence
      RETURNING
        VALUE(result) TYPE i.
    METHODS is_subtotalled
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS set_sequence
      IMPORTING
        value TYPE i DEFAULT zif_uitb_c_alv_sorting=>ascending
      RAISING
        zcx_uitb_alv_error.
    METHODS set_subtotal
      IMPORTING
        value TYPE abap_bool DEFAULT abap_true.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mr_column TYPE REF TO ZCL_UITB_alv_column.
    DATA mv_sequence TYPE i.
    DATA mf_subtotal TYPE abap_bool.
ENDCLASS.



CLASS ZCL_UITB_ALV_SORT IMPLEMENTATION.


  METHOD constructor.
    super->constructor(
        io_controller = ir_controller
        iv_name       = 'SORT'
    ).
    mv_sequence = iv_sequence.
    mf_subtotal = if_subtotal.
    mr_column = ir_column.
  ENDMETHOD.


  METHOD get_column_name.
    result = mr_column->get_name( ).
  ENDMETHOD.


  METHOD get_sequence.
    result = mv_sequence.
  ENDMETHOD.


  METHOD is_subtotalled.
    result = mf_subtotal.
  ENDMETHOD.


  METHOD set_sequence.
    CHECK value <> mv_sequence.

    mv_sequence = value.

    set_setter_changed( iv_method = 'SET_SEQUENCE' ).
  ENDMETHOD.


  METHOD set_subtotal.
    CHECK value <> mf_subtotal.

    mf_subtotal = value.

    set_setter_changed( iv_method = 'SET_SUBTOTAL' ).
  ENDMETHOD.
ENDCLASS.
