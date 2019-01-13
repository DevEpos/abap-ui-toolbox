CLASS zcl_utib_custom_value_help DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_projection TYPE string
        iv_from       TYPE string
        iv_order_by   TYPE string OPTIONAL.
    METHODS call_value_help.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS select_data.
    METHODS pre_filter_data.
ENDCLASS.



CLASS zcl_utib_custom_value_help IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD call_value_help.

  ENDMETHOD.

  METHOD select_data.

  ENDMETHOD.

  METHOD pre_filter_data.

  ENDMETHOD.

ENDCLASS.
