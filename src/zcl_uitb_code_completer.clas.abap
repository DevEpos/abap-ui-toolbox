"! <p class="shorttext synchronized" lang="en">ABAP code completer</p>
CLASS zcl_uitb_code_completer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Retrieve code completion results</p>
    "!
    CLASS-METHODS get_completion_results
      IMPORTING
        it_source                    TYPE rswsourcet
        iv_pos_x                     TYPE i
        iv_pos_y                     TYPE i
      RETURNING
        VALUE(rt_completion_results) TYPE scc_adt_completion_results.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_uitb_code_completer IMPLEMENTATION.

  METHOD get_completion_results.
    DATA(lv_pos_x) = iv_pos_x.


    DATA(lt_source) = it_source.
*.. Add additional lines if line number does not match the lines of the source code
    WHILE iv_pos_y > lines( lt_source ).
      lt_source = VALUE #( BASE lt_source ( ) ).
    ENDWHILE.
*.. Adjust column position depending on line content
    DATA(lr_source) = REF #( lt_source[ iv_pos_y ] ).
    DATA(lv_pos_x_source_length) = strlen( lr_source->* ).
    IF lr_source->* IS INITIAL.
      lv_pos_x = 0.
    ENDIF.
    IF lv_pos_x < 0.
      lv_pos_x = 0.
    ENDIF.

    IF lv_pos_x_source_length < lv_pos_x.
      DATA(lv_fill_spaces) = lv_pos_x - lv_pos_x_source_length.
      DO lv_fill_spaces TIMES.
        lr_source->* = |{ lr_source->* } |.
      ENDDO.
    ENDIF.



    DATA(lo_request) = NEW lcl_completion_request(
      it_source_code = lt_source
      iv_position_x  = lv_pos_x
      iv_position_y  = iv_pos_y
    ).
    DATA(lo_response) = NEW lcl_completion_response( ).

*.. Call custom implementation of ADT code completion
    DATA(lo_cc_resource_app) = NEW lcl_adt_cc( ).
    TRY.
        lo_cc_resource_app->post(
            request  = lo_request
            response = lo_response
        ).
        rt_completion_results = lo_response->mt_completion_results.
      CATCH cx_adt_rest.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
