CLASS ZCL_UITB_alv_dropdown DEFINITION
  PUBLIC
  INHERITING FROM ZCL_UITB_alv_metadata
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS ZCL_UITB_alv_grid_adapter.

  PUBLIC SECTION.
    METHODS get_handle
      RETURNING
        VALUE(result) TYPE i .
    METHODS get_values
      RETURNING
        VALUE(result) TYPE lvc_t_dral.
    METHODS set_values
      IMPORTING
        value TYPE ZIF_UITB_alv_types=>tt_dropdown_value.
    METHODS constructor
      IMPORTING
        iv_handle     TYPE i
        it_values     TYPE ZIF_UITB_alv_types=>tt_dropdown_value OPTIONAL
        ir_controller TYPE REF TO ZIF_UITB_alv_metadata_ctrller.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_handle.
    DATA mt_values TYPE lvc_t_dral.
ENDCLASS.



CLASS ZCL_UITB_alv_dropdown IMPLEMENTATION.
  METHOD get_handle.
    result = mv_handle.
  ENDMETHOD.

  METHOD get_values.
    result = mt_values.
  ENDMETHOD.

  METHOD set_values.
    mt_values = VALUE #(
      FOR <ls_value> IN value
      ( handle     = mv_handle
        int_value  = <ls_value>-int_value
        value      = <ls_value>-value )
    ).

    set_setter_changed( iv_method = 'SET_VALUES' ).

  ENDMETHOD.

  METHOD constructor.
    super->constructor(
      io_controller = ir_controller
      iv_name       = 'DROPDOWN'
    ).

    mv_handle = iv_handle.
    set_values( it_values ).
  ENDMETHOD.

ENDCLASS.
