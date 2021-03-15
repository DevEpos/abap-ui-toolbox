*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_pretty_printer_settings DEFINITION
 INHERITING FROM cl_pretty_printer_wb_settings.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_line_length TYPE i.
    METHODS if_pretty_printer_settings~get_case_mode
        REDEFINITION.
  PRIVATE SECTION.
    DATA mv_line_length TYPE i.
ENDCLASS.
