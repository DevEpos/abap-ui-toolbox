"! <p class="shorttext synchronized" lang="en">Popup for data input</p>
INTERFACE zif_uitb_pgv_popup
  PUBLIC .

  TYPES:
    ty_validation_mode TYPE c LENGTH 1,
    "! <p class="shorttext synchronized" lang="en">Validation configuration for field validation</p>
    BEGIN OF ty_validation_config,
      comparator TYPE ddoption,
      value_low  TYPE c LENGTH 132,
      value_high TYPE c LENGTH 132,
    END OF ty_validation_config,
    "! <p class="shorttext synchronized" lang="en">List of validation configurations</p>
    tt_validation_config TYPE STANDARD TABLE OF ty_validation_config WITH EMPTY KEY.

  "! <p class="shorttext synchronized" lang="en">Field on popup dialog</p>
  TYPES BEGIN OF ty_dialog_field_ext.
  INCLUDE TYPE sval.
  TYPES validation TYPE zif_uitb_pgv_popup=>ty_validation_config.
  TYPES END OF ty_dialog_field_ext.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">List of dialog extended fields</p>
    tt_dialog_field_ext TYPE STANDARD TABLE OF ty_dialog_field_ext WITH EMPTY KEY,
    "! <p class="shorttext synchronized" lang="en">List of dialog fields</p>
    tt_dialog_field     TYPE STANDARD TABLE OF sval WITH EMPTY KEY.

  CONSTANTS:
    BEGIN OF c_display_attributes,
      normal                TYPE spo_fattr VALUE '',
      highlighted           TYPE spo_fattr VALUE '01',
      read_only             TYPE spo_fattr VALUE '02',
      highlighted_read_only TYPE spo_fattr VALUE '03',
      hidden                TYPE spo_fattr VALUE '04',
    END OF c_display_attributes.

  CONSTANTS:
    BEGIN OF c_validation_modes,
      "! No validation beyond the normal data type validation - if it
      "! is was not deactivated
      no_custom_validation TYPE ty_validation_mode VALUE '',
      "! The fields are checked via event registration. <br/>
      "! For this the caller should implement a local event handler for the
      "! Event <strong>check_field_values</strong> in class {@link zcl_uitb_pgv_exit_events}
      local_user_check     TYPE ty_validation_mode VALUE 'l',
      "! Each field can have a custom validation. This validation is kind of simple
      "! and consist of a comparator and and one or two compare values. <br/>
      "! The possible values of the comparator can be checked in domain <strong>ddoption</strong>
      inline_user_check    TYPE ty_validation_mode VALUE 'i',
    END OF c_validation_modes.

  "! <p class="shorttext synchronized" lang="en">Adds new field to the popup</p>
  "! @parameter iv_tab | The table where the field exists
  "! @parameter iv_name | The field name
  "! @parameter iv_label | The label for the field on the screen
  "! @parameter iv_value | The initial value of the field
  "! @parameter iv_screen_attr | Screen attribute of the field
  "! <br/>see <strong>zif_uitb_pgv_popup=>c_display_attributes</strong> for possible values
  "! @parameter if_obligatory | 'X' =  a value is required for the field
  "! @parameter if_no_value_help | 'X' = no value help button will be displayed
  "! @parameter is_validation | Optional validation configuration for the field
  METHODS add_field
    IMPORTING
      iv_name          TYPE fieldname
      iv_tab           TYPE tabname
      iv_label         TYPE scrtext_m OPTIONAL
      iv_value         TYPE spo_value OPTIONAL
      iv_screen_attr   TYPE spo_fattr OPTIONAL
      if_obligatory    TYPE abap_bool OPTIONAL
      if_no_value_help TYPE abap_bool OPTIONAL
      is_validation    TYPE ty_validation_config OPTIONAL
    RETURNING
      VALUE(ro_popup)  TYPE REF TO zif_uitb_pgv_popup.

  "! <p class="shorttext synchronized" lang="en">Adds list of fields to dialog</p>
  "! @parameter it_fields | List of fields for dialog
  METHODS add_fields
    IMPORTING
      it_fields       TYPE tt_dialog_field_ext
    RETURNING
      VALUE(ro_popup) TYPE REF TO zif_uitb_pgv_popup.

  "! <p class="shorttext synchronized" lang="en">Displays the popup dialog</p>
  METHODS show
    IMPORTING
      iv_start_col    TYPE i DEFAULT 12
      iv_start_row    TYPE i DEFAULT 5
    RETURNING
      VALUE(ro_popup) TYPE REF TO zif_uitb_pgv_popup.

  "! <p class="shorttext synchronized" lang="en">Retrieves the new table field value</p>
  METHODS get_field_value
    IMPORTING
      iv_name          TYPE fieldname
      iv_tab           TYPE tabname
    RETURNING
      VALUE(rv_result) TYPE spo_value
    RAISING
      cx_sy_itab_line_not_found.

  "! <p class="shorttext synchronized" lang="en">Retrieves field value by table index</p>
  METHODS get_field_val_by_index
    IMPORTING
      iv_index         TYPE i
    RETURNING
      VALUE(rv_result) TYPE spo_value
    RAISING
      cx_sy_itab_line_not_found.

  "! <p class="shorttext synchronized" lang="en">Returns value of first field</p>
  METHODS get_first_field_value
    RETURNING
      VALUE(rv_result) TYPE spo_value
    RAISING
      cx_sy_itab_line_not_found.

  "! <p class="shorttext synchronized" lang="en">Returns 'X' if the dialog was cancelled</p>
  METHODS cancelled
    RETURNING
      VALUE(rf_cancelled) TYPE abap_bool.
ENDINTERFACE.
