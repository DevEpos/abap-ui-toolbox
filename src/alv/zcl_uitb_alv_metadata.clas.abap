CLASS ZCL_UITB_alv_metadata DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_controller TYPE REF TO ZIF_UITB_alv_metadata_ctrller
        iv_name       TYPE string OPTIONAL .
  PROTECTED SECTION.
    DATA mr_controller TYPE REF TO ZIF_UITB_alv_metadata_ctrller.
    DATA mv_name TYPE string.

    METHODS set_setter_changed
      IMPORTING
        iv_method TYPE string.
private section.
ENDCLASS.



CLASS ZCL_UITB_ALV_METADATA IMPLEMENTATION.


  METHOD constructor.
    mr_controller = io_controller.
    mv_name = iv_name.
  ENDMETHOD.


  METHOD set_setter_changed.
    mr_controller->set_changed(
        iv_name         = mv_name
        iv_flavour      = zif_uitb_c_alv_chglist_flavor=>setter
        iv_refresh_mode = zif_uitb_c_alv_refresh=>soft
        iv_method       = iv_method
    ).
  ENDMETHOD.
ENDCLASS.
