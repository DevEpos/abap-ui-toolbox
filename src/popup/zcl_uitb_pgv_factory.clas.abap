"! <p class="shorttext synchronized" lang="en">Factory for creating popups for data input</p>
CLASS zcl_uitb_pgv_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Creates new popup</p>
    CLASS-METHODS create_popup
      IMPORTING
        iv_title           TYPE string
        iv_validation_mode TYPE zif_uitb_pgv_popup=>ty_validation_mode OPTIONAL
      RETURNING
        VALUE(ro_popup)    TYPE REF TO zif_uitb_pgv_popup.

    "! <p class="shorttext synchronized" lang="en">Creates new popup with one field</p>
    CLASS-METHODS create_single_field_popup
      IMPORTING
        iv_title           TYPE string
        is_field           TYPE zif_uitb_pgv_popup=>ty_dialog_field_ext
        iv_validation_mode TYPE zif_uitb_pgv_popup=>ty_validation_mode OPTIONAL
      RETURNING
        VALUE(ro_popup)    TYPE REF TO zif_uitb_pgv_popup.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_uitb_pgv_factory IMPLEMENTATION.

  METHOD create_popup.
    ro_popup = NEW zcl_uitb_pgv_popup(
      iv_title           = iv_title
      iv_validation_mode = iv_validation_mode ).
  ENDMETHOD.

  METHOD create_single_field_popup.
    ro_popup = NEW zcl_uitb_pgv_popup(
        iv_title           = iv_title
        iv_validation_mode = iv_validation_mode
      )->zif_uitb_pgv_popup~add_field(
        iv_name          = is_field-fieldname
        iv_tab           = is_field-tabname
        iv_label         = is_field-fieldtext
        iv_value         = is_field-value
        iv_screen_attr   = is_field-field_attr
        if_obligatory    = is_field-field_obl
        if_no_value_help = is_field-novaluehlp
        is_validation    = is_field-validation ).
  ENDMETHOD.

ENDCLASS.
