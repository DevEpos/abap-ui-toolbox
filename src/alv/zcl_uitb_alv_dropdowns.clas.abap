CLASS ZCL_UITB_alv_dropdowns DEFINITION
  PUBLIC
  INHERITING FROM ZCL_UITB_alv_metadata
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS get_count
      RETURNING
        VALUE(result) TYPE i .
    METHODS add_dropdown
      IMPORTING
        iv_handle    TYPE i
        it_values    TYPE ZIF_UITB_alv_types=>tt_dropdown_value
      RETURNING
        VALUE(value) TYPE REF TO ZCL_UITB_alv_dropdown
      RAISING
        ZCX_UITB_alv_existing .
    METHODS remove_dropdown
      IMPORTING
        iv_handle TYPE i.
    METHODS clear .
    METHODS get_dropdown
      IMPORTING
        iv_handle     TYPE i
      RETURNING
        VALUE(result) TYPE REF TO ZCL_UITB_alv_dropdown.
    METHODS get
      RETURNING
        VALUE(result) TYPE ZIF_UITB_alv_types=>tt_dropdown_ref.
    METHODS constructor
      IMPORTING
        ir_controller TYPE REF TO ZIF_UITB_alv_metadata_ctrller.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_dropdown TYPE ZIF_UITB_alv_types=>tt_dropdown_ref.

    METHODS raise_dropdown_exists
      IMPORTING
        iv_method TYPE any OPTIONAL
        iv_handle TYPE any OPTIONAL
      RAISING
        ZCX_UITB_alv_existing.
ENDCLASS.



CLASS ZCL_UITB_alv_dropdowns IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
      ir_controller = ir_controller
      iv_name       = 'DROPDOWNS'
    ).
  ENDMETHOD.

  METHOD get_count.
    result = lines( mt_dropdown ).
  ENDMETHOD.

  METHOD add_dropdown.
    IF line_exists( mt_dropdown[ handle = iv_handle ] ).
      raise_dropdown_exists(
        iv_method = 'ADD'
        iv_handle = iv_handle
      ).
    ENDIF.

    " dropdown does not exist yet, it can be added ->
    mt_dropdown = value #(
      base mt_dropdown
      ( handle = iv_handle
        dropdown_ref = new #(
            iv_handle     = iv_handle
            it_values     = it_values
            ir_controller = mr_controller
        )
      )
    ).
  ENDMETHOD.

  METHOD remove_dropdown.
    DELETE mt_dropdown WHERE handle = iv_handle.

    set_setter_changed( iv_method = 'REMOVE' ).
  ENDMETHOD.

  METHOD clear.
    CLEAR mt_dropdown.
    set_setter_changed( iv_method = 'CLEAR' ).
  ENDMETHOD.

  METHOD get_dropdown.
    result = mt_dropdown[ handle = iv_handle ]-dropdown_ref.
  ENDMETHOD.

  METHOD get.
    result = mt_dropdown.
  ENDMETHOD.


  METHOD raise_dropdown_exists.
    RAISE EXCEPTION TYPE ZCX_UITB_alv_existing
      EXPORTING
        msgv1 = |{ 'Dropdown' }|
        msgv2 = |{ iv_handle }|
        msgv3 = |{ CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( me ) )->absolute_name }|
        msgv4 = |{ iv_method }|.
  ENDMETHOD.

ENDCLASS.
