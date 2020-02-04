CLASS ZCL_UITB_alv_filter DEFINITION
  PUBLIC
  INHERITING FROM ZCL_UITB_alv_metadata
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS ZCL_UITB_alv_grid_adapter.

  PUBLIC SECTION.

    METHODS add_selopt
      IMPORTING
        iv_sign   TYPE selopt-sign DEFAULT 'I'
        iv_option TYPE selopt-option DEFAULT 'EQ'
        iv_low    TYPE zuitb_generic_range-low OPTIONAL
        iv_high   TYPE zuitb_generic_range-high OPTIONAL.
*    returning
*      value(VALUE) type ref to CL_SALV_SELOPT
*    raising
*      CX_SALV_DATA_ERROR .
    METHODS clear .
    METHODS get
      RETURNING
        VALUE(result) TYPE zuitb_generic_range_itab .
    METHODS get_columnname
      RETURNING
        VALUE(result) TYPE lvc_fname .
    METHODS constructor
      IMPORTING
        ir_column     TYPE REF TO ZCL_UITB_alv_column
        ir_controller TYPE REF TO ZIF_UITB_alv_metadata_ctrller OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mr_column TYPE REF TO ZCL_UITB_alv_column.
    DATA mt_selopts TYPE zuitb_generic_range_itab.
ENDCLASS.



CLASS ZCL_UITB_ALV_FILTER IMPLEMENTATION.


  METHOD add_selopt .

    mt_selopts = VALUE #( BASE mt_selopts
      ( sign   = iv_sign
        option = iv_option
        low    = iv_low
        high   = iv_high )
    ).

    SORT mt_selopts BY sign option low high.
    DELETE ADJACENT DUPLICATES FROM mt_selopts.

    CHECK mr_controller IS BOUND.

    mr_controller->set_changed(
      iv_name         = mv_name
      ir_ref          = me
      iv_flavour      = zif_uitb_c_alv_chglist_flavor=>setter
      iv_refresh_mode = zif_uitb_c_alv_refresh=>full
      iv_method       = 'ADD' ).

  ENDMETHOD.


  METHOD clear.

    CLEAR mt_selopts.

    CHECK mr_controller IS BOUND.

    mr_controller->set_changed(
      iv_name         = mv_name
      ir_ref          = me
      iv_flavour      = zif_uitb_c_alv_chglist_flavor=>setter
      iv_refresh_mode = zif_uitb_c_alv_refresh=>full
      iv_method       = 'CLEAR' ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor(
      io_controller = ir_controller
      iv_name       = 'FILTER' ).

    mr_column = ir_column.

  ENDMETHOD.


  METHOD get .

    result = mt_selopts.

  ENDMETHOD.


  METHOD get_columnname.

    result = mr_column->get_name( ).

  ENDMETHOD.
ENDCLASS.
