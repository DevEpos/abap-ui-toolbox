INTERFACE ZIF_UITB_alv_metadata_ctrller
  PUBLIC .

  METHODS register
    IMPORTING
      !iv_name TYPE any
      !ir_ref  TYPE REF TO ZCL_UITB_alv_metadata OPTIONAL .
  METHODS set_changed
    IMPORTING
      !iv_name         TYPE any
      !iv_flavour      TYPE i DEFAULT zif_uitb_c_alv_chglist_flavor=>register
      !iv_refresh_mode TYPE i DEFAULT zif_uitb_c_alv_refresh=>none
      !iv_method       TYPE any OPTIONAL
      !iv_object       TYPE any OPTIONAL
      !ir_ref          TYPE REF TO ZCL_UITB_alv_metadata OPTIONAL
      !iv_frontend     TYPE sap_bool OPTIONAL .
  METHODS clear_changelist
    IMPORTING
      iv_name         TYPE any OPTIONAL
      iv_object       TYPE any OPTIONAL
      iv_method       TYPE any OPTIONAL
      if_change       TYPE abap_bool OPTIONAL
      iv_flavour      TYPE i OPTIONAL
      iv_refresh_mode TYPE i OPTIONAL
      it_range        TYPE zuitb_generic_range_itab OPTIONAL
    RETURNING
      VALUE(boolean)  TYPE sap_bool .
INTERFACE if_salv_c_bool_sap LOAD .
  METHODS check_changelist
    IMPORTING
      iv_name          TYPE any OPTIONAL
      iv_object        TYPE any OPTIONAL
      iv_method        TYPE any OPTIONAL
      iv_flavour       TYPE i OPTIONAL
      iv_refresh_mode  TYPE i DEFAULT zif_uitb_c_alv_refresh=>none
      if_change        TYPE abap_bool DEFAULT abap_true
      it_range         TYPE zuitb_generic_range_itab OPTIONAL
    RETURNING
      VALUE(rf_exists) TYPE abap_bool .
  METHODS get_changelist
    IMPORTING
      iv_flavour    TYPE salv_de_constant DEFAULT zif_uitb_c_alv_chglist_flavor=>register
    RETURNING
      VALUE(result) TYPE ZIF_UITB_alv_types=>tt_alv_changelist .
ENDINTERFACE.
