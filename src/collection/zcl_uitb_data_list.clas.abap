class ZCL_UITB_DATA_LIST definition
  public
  final
  create public .

public section.

  interfaces ZIF_UITB_DATA_REF_LIST .

  methods CONSTRUCTOR
    importing
      !IR_T_DATA type ref to DATA
      !IT_COMP_EXTEND type ZCL_UITB_RTTI_UTIL=>TT_COMP_TYPE optional .
  methods EXTEND_TABLE
    importing
      !IT_COMP_EXTEND type ZCL_UITB_RTTI_UTIL=>TT_COMP_TYPE optional .
  PROTECTED SECTION.
    METHODS describe_table.
  PRIVATE SECTION.
    DATA mr_t_list TYPE REF TO data.
    DATA mr_line_type TYPE REF TO cl_abap_structdescr.
    DATA mr_table_type TYPE REF TO cl_abap_tabledescr.
ENDCLASS.



CLASS ZCL_UITB_DATA_LIST IMPLEMENTATION.


  METHOD CONSTRUCTOR.
    mr_t_list = ir_t_data.
    describe_table( ).

    IF it_comp_extend IS NOT INITIAL.
      extend_table( it_comp_extend ).
    ENDIF.
  ENDMETHOD.


  METHOD DESCRIBE_TABLE.
    assign_table mr_t_list table.

    zcl_uitb_rtti_util=>describe_table_by_data(
      EXPORTING it_data       = <lt_table>
      IMPORTING er_line_type  = mr_line_type
                er_table_type = mr_table_type
    ).
  ENDMETHOD.


  METHOD EXTEND_TABLE.
    assign_table mr_t_list table.

    zcl_uitb_rtti_util=>extend_struct_by_components(
      EXPORTING ir_struct_descr     = mr_line_type
                it_component_append = it_comp_extend
      IMPORTING er_line_type        = mr_line_type
                er_table_type       = mr_table_type
    ).

    " create the new table
    create_table extended.

    " move old data to new table
    MOVE-CORRESPONDING <lt_table> TO <lt_extended>.

    mr_t_list = lr_t_extended.
  ENDMETHOD.


  METHOD ZIF_UITB_DATA_REF_LIST~ADD.
    assign_table  mr_t_list     table.
    assign_struct ir_s_element  element.

    create_line new.

    MOVE-CORRESPONDING <ls_element> TO <ls_new>.

    <lt_table> = VALUE #( BASE <lt_table> ( <ls_new> ) ).
  ENDMETHOD.


  METHOD ZIF_UITB_DATA_REF_LIST~ADD_LIST.
    assign_table ir_t_list new_list.
    assign_table mr_t_list existing_list.

    LOOP AT <lt_new_list> ASSIGNING FIELD-SYMBOL(<ls_new_element>).
      create_line new_line.
      MOVE-CORRESPONDING <ls_new_element> TO <ls_new_line>.

      APPEND <ls_new_line> TO <lt_existing_list>.
    ENDLOOP.
  ENDMETHOD.


  METHOD ZIF_UITB_DATA_REF_LIST~CLEAR.
    assign_table mr_t_list table.
    CLEAR <lt_table>.
  ENDMETHOD.


  METHOD ZIF_UITB_DATA_REF_LIST~CREATE_NEW_LINE.
    create_line new.

    assign_table mr_t_list table.
    APPEND <ls_new> TO <lt_table> ASSIGNING FIELD-SYMBOL(<ls_added>).

    rr_new_element = REF #( <ls_added> ).
  ENDMETHOD.


  METHOD ZIF_UITB_DATA_REF_LIST~GET_ALL.
    rr_t_data = mr_t_list.
  ENDMETHOD.


  METHOD ZIF_UITB_DATA_REF_LIST~GET_ELEMENT.
    assign_table mr_t_list table.

    TRY .
        rr_element = REF #( <lt_table>[ iv_index ] ).
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_uitb_element_not_found
          EXPORTING
            textid = zcx_uitb_element_not_found=>index_access
            index  = iv_index.
    ENDTRY.
  ENDMETHOD.


  METHOD ZIF_UITB_DATA_REF_LIST~GET_ITERATOR.
    rr_iterator = zcl_uitb_data_ref_iterator=>create(
        ir_list     = me
        iv_where    = iv_where
    ).
  ENDMETHOD.


  METHOD zif_uitb_data_ref_list~has_component.
    DATA(lt_components) = mr_line_type->get_components( ).
    rf_exists = xsdbool( line_exists( lt_components[ name = iv_fieldname ] ) ).
  ENDMETHOD.


  METHOD ZIF_UITB_DATA_REF_LIST~SIZE.
    assign_table mr_t_list table.
    rv_size = lines( <lt_table> ).
  ENDMETHOD.
ENDCLASS.
