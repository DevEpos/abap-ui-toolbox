*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_adt_cc IMPLEMENTATION.

  METHOD determine_input_data.

    TRY.
        DATA(lo_cc_request) = CAST lcl_completion_request( request ).
        xpos = lo_cc_request->mv_position_x.
        ypos = lo_cc_request->mv_position_y.
        include = 'ZTEST'.
        source = lo_cc_request->mt_source_code.
      CATCH cx_sy_move_cast_error.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_completion_response IMPLEMENTATION.

  METHOD if_adt_rest_response~get_inner_rest_response.
  ENDMETHOD.

  METHOD if_adt_rest_response~set_body_data.
    mt_completion_results = data.
  ENDMETHOD.

  METHOD if_adt_rest_response~set_etag.
  ENDMETHOD.

  METHOD if_adt_rest_response~set_last_modified.
  ENDMETHOD.

  METHOD if_adt_rest_response~set_location.
  ENDMETHOD.

  METHOD if_adt_rest_response~set_status.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_completion_request IMPLEMENTATION.

  METHOD constructor.
    mv_position_x = iv_position_x.
    mv_position_y = iv_position_y.
    mt_source_code = it_source_code.
  ENDMETHOD.

  METHOD if_adt_rest_request~get_accept_content_type.

  ENDMETHOD.

  METHOD if_adt_rest_request~get_body_data.
    data = mt_source_code.
  ENDMETHOD.

  METHOD if_adt_rest_request~get_content_type.

  ENDMETHOD.

  METHOD if_adt_rest_request~get_if_match.

  ENDMETHOD.

  METHOD if_adt_rest_request~get_if_none_match.

  ENDMETHOD.

  METHOD if_adt_rest_request~get_inner_rest_request.

  ENDMETHOD.

  METHOD if_adt_rest_request~get_uri_attribute.

  ENDMETHOD.

  METHOD if_adt_rest_request~get_uri_fragment.

  ENDMETHOD.

  METHOD if_adt_rest_request~get_uri_query_parameter.
    IF name = cl_cc_adt_res_code_completion=>co_param_signal_completeness.
      value = 'false'.
    ENDIF.
  ENDMETHOD.

  METHOD if_adt_rest_request~get_uri_query_parameter_values.

  ENDMETHOD.

ENDCLASS.
