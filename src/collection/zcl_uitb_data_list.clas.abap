CLASS zcl_uitb_data_list DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_uitb_data_ref_list .

    "! <p class="shorttext synchronized" lang="en">Creates new list for given table type name</p>
    CLASS-METHODS create_for_table_type
      IMPORTING
        iv_table_type       TYPE tabname
        !it_comp_extend     TYPE zcl_uitb_rtti_util=>tt_comp_type OPTIONAL
      RETURNING
        VALUE(rr_data_list) TYPE REF TO zif_uitb_data_ref_list.
    "! <p class="shorttext synchronized" lang="en">Creates new list for given table name</p>
    CLASS-METHODS create_for_table_name
      IMPORTING
        iv_table            TYPE tabname
        it_comp_extend      TYPE zcl_uitb_rtti_util=>tt_comp_type OPTIONAL
      RETURNING
        VALUE(rr_data_list) TYPE REF TO zif_uitb_data_ref_list.
    "! <p class="shorttext synchronized" lang="en">Creates new list for given table ref</p>
    CLASS-METHODS create_for_table_ref
      IMPORTING
        ir_t_data           TYPE REF TO data
        it_comp_extend      TYPE zcl_uitb_rtti_util=>tt_comp_type OPTIONAL
      RETURNING
        VALUE(rr_data_list) TYPE REF TO zif_uitb_data_ref_list.
  PROTECTED SECTION.
    METHODS describe_table.
  PRIVATE SECTION.
    DATA mr_t_list TYPE REF TO data.
    DATA mr_line_type TYPE REF TO cl_abap_structdescr.
    DATA mr_table_type TYPE REF TO cl_abap_tabledescr.
ENDCLASS.



CLASS zcl_uitb_data_list IMPLEMENTATION.

  METHOD create_for_table_name.
    DATA(lr_data_list) = NEW zcl_uitb_data_list( ).

    zcl_uitb_rtti_util=>create_table_for_table(
      EXPORTING iv_tabname    = iv_table
      IMPORTING er_table_data = lr_data_list->mr_t_list
                er_line_type  = lr_data_list->mr_line_type
                er_table_type = lr_data_list->mr_table_type
    ).

    IF it_comp_extend IS NOT INITIAL.
      lr_data_list->zif_uitb_data_ref_list~extend( it_comp_extend ).
    ENDIF.

    rr_data_list = lr_data_list.
  ENDMETHOD.

  METHOD create_for_table_type.
    DATA(lr_data_list) = NEW zcl_uitb_data_list( ).

    zcl_uitb_rtti_util=>create_table_for_table_type(
      EXPORTING iv_table_type_name = iv_table_type
      IMPORTING er_table_data      = lr_data_list->mr_t_list
                er_line_type       = lr_data_list->mr_line_type
                er_table_type      = lr_data_list->mr_table_type
    ).

    IF it_comp_extend IS NOT INITIAL.
      lr_data_list->zif_uitb_data_ref_list~extend( it_comp_extend ).
    ENDIF.

    rr_data_list = lr_data_list.
  ENDMETHOD.

  METHOD create_for_table_ref.
    DATA(lr_data_list) = NEW zcl_uitb_data_list( ).

    lr_data_list->mr_t_list = ir_t_data.
    lr_data_list->describe_table( ).

    IF it_comp_extend IS NOT INITIAL.
      lr_data_list->zif_uitb_data_ref_list~extend( it_comp_extend ).
    ENDIF.

    rr_data_list = lr_data_list.
  ENDMETHOD.

  METHOD describe_table.
    assign_table mr_t_list table.

    zcl_uitb_rtti_util=>describe_table_by_data(
      EXPORTING it_data       = <lt_table>
      IMPORTING er_line_type  = mr_line_type
                er_table_type = mr_table_type
    ).
  ENDMETHOD.

  METHOD zif_uitb_data_ref_list~fill_component.
    ASSIGN ir_s_element->* TO FIELD-SYMBOL(<ls_element>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN COMPONENT iv_comp_name OF STRUCTURE <ls_element> TO FIELD-SYMBOL(<lv_value>).
    IF sy-subrc = 0.
      IF if_use_corresponding = abap_true.
        MOVE-CORRESPONDING iv_value TO <lv_value>.
      ELSE.
        <lv_value> = iv_value.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD zif_uitb_data_ref_list~extend.
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


  METHOD zif_uitb_data_ref_list~add.
    assign_table  mr_t_list     table.
    assign_struct ir_s_element  element.

    create_line new.

    IF if_expand_nested_tables = abap_true.
      MOVE-CORRESPONDING <ls_element> TO <ls_new> EXPANDING NESTED TABLES.
    ELSE.
      MOVE-CORRESPONDING <ls_element> TO <ls_new>.
    ENDIF.

    <lt_table> = VALUE #( BASE <lt_table> ( <ls_new> ) ).
  ENDMETHOD.


  METHOD zif_uitb_data_ref_list~add_list.
    assign_table ir_t_list new_list.
    assign_table mr_t_list existing_list.

    LOOP AT <lt_new_list> ASSIGNING FIELD-SYMBOL(<ls_new_element>).
      create_line new_line.
      IF if_expand_nested_tables = abap_true.
        MOVE-CORRESPONDING <ls_new_element> TO <ls_new_line> EXPANDING NESTED TABLES.
      ELSE.
        MOVE-CORRESPONDING <ls_new_element> TO <ls_new_line>.
      ENDIF.

      APPEND <ls_new_line> TO <lt_existing_list>.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_uitb_data_ref_list~clear.
    assign_table mr_t_list table.
    CLEAR <lt_table>.
  ENDMETHOD.


  METHOD zif_uitb_data_ref_list~create_new_line.
    create_line new.

    assign_table mr_t_list table.
    APPEND <ls_new> TO <lt_table> ASSIGNING FIELD-SYMBOL(<ls_added>).

    rr_new_element = REF #( <ls_added> ).
  ENDMETHOD.


  METHOD zif_uitb_data_ref_list~get_all.
    rr_t_data = mr_t_list.
  ENDMETHOD.


  METHOD zif_uitb_data_ref_list~get_element.
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


  METHOD zif_uitb_data_ref_list~get_iterator.
    rr_iterator = zcl_uitb_data_ref_iterator=>create(
        ir_list     = me
        iv_where    = iv_where
    ).
  ENDMETHOD.


  METHOD zif_uitb_data_ref_list~has_component.
    DATA(lt_components) = mr_line_type->get_components( ).
    rf_exists = xsdbool( line_exists( lt_components[ name = iv_fieldname ] ) ).
  ENDMETHOD.


  METHOD zif_uitb_data_ref_list~size.
    assign_table mr_t_list table.
    rv_size = lines( <lt_table> ).
  ENDMETHOD.
ENDCLASS.
