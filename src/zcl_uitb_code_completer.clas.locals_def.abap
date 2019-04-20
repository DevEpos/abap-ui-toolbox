*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_adt_cc DEFINITION
INHERITING FROM cl_cc_adt_res_code_completion.

  PROTECTED SECTION.
    METHODS determine_input_data
        REDEFINITION.
ENDCLASS.

CLASS lcl_completion_request DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_adt_rest_request.

    METHODS constructor
      IMPORTING
        it_source_code TYPE rswsourcet
        iv_position_x  TYPE i
        iv_position_y  TYPE i.

    DATA mt_source_code TYPE rswsourcet READ-ONLY.
    DATA mv_position_x TYPE i READ-ONLY.
    DATA mv_position_y TYPE i READ-ONLY.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_completion_response DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_adt_rest_response.

    DATA mt_completion_results TYPE scc_adt_completion_results READ-ONLY.
ENDCLASS.
