CLASS zcl_uitb_alv_data_descr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS describe_table
      IMPORTING
        ir_columns TYPE REF TO zcl_uitb_alv_columns
        ir_table   TYPE REF TO data .
    CLASS-METHODS read_structdescr
      IMPORTING
        ir_structdescr  TYPE REF TO cl_abap_structdescr
        iv_language     TYPE langu DEFAULT sy-langu
      RETURNING
        VALUE(rt_dfies) TYPE ddfields .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS fill_column
      IMPORTING
        ir_columns TYPE REF TO zcl_uitb_alv_columns
        it_dfies   TYPE ddfields .
    CLASS-METHODS transform
      CHANGING
        !ct_dfies TYPE ddfields OPTIONAL
        !cs_dfies TYPE dfies OPTIONAL .
ENDCLASS.



CLASS ZCL_UITB_ALV_DATA_DESCR IMPLEMENTATION.


  METHOD describe_table .

    DATA: lr_tabdescr TYPE REF TO cl_abap_structdescr,
          lt_dfies    TYPE ddfields,
          lr_data     TYPE REF TO data.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    ir_columns->zif_uitb_list~clear( ).

    ASSIGN ir_table->* TO <lt_table>.
    CREATE DATA lr_data LIKE LINE OF <lt_table>.
    ir_columns->mr_table_structure = lr_data.

    lr_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_data ).

    lt_dfies = read_structdescr(
        ir_structdescr = lr_tabdescr
        iv_language    = ir_columns->get_description_language( )
    ).

    fill_column( it_dfies   = lt_dfies
                 ir_columns = ir_columns ).

  ENDMETHOD.


  METHOD fill_column.

    DATA: ls_dfies       TYPE dfies.

    LOOP AT it_dfies INTO ls_dfies.
      DATA(ls_fieldcat) = VALUE lvc_s_fcat(
        fieldname       = ls_dfies-fieldname
        ref_table       = ls_dfies-reftable
        ref_field       = ls_dfies-reffield
        rollname        = ls_dfies-rollname
        dd_roll         = ls_dfies-rollname
        datatype        = ls_dfies-datatype
        inttype         = ls_dfies-inttype
        intlen          = ls_dfies-leng
        dd_outlen       = ls_dfies-outputlen
        no_sign         = COND #( WHEN ls_dfies-sign = abap_false THEN abap_true )
        key             = ls_dfies-keyflag
        lowercase       = ls_dfies-lowercase
        reptext         = ls_dfies-reptext
        scrtext_s       = ls_dfies-scrtext_s
        scrtext_m       = ls_dfies-scrtext_m
        scrtext_l       = ls_dfies-scrtext_l
        domname         = ls_dfies-domname
        f4availabl      = ls_dfies-f4availabl
        decimals        = ls_dfies-decimals
        convexit        = ls_dfies-convexit
      ).

      ir_columns->add_column( is_data = ls_fieldcat ).

    ENDLOOP.

  ENDMETHOD.


  METHOD read_structdescr.

    DATA: ls_component TYPE abap_componentdescr,
          lr_struct    TYPE REF TO cl_abap_structdescr,
          lr_element   TYPE REF TO cl_abap_elemdescr,
          lt_sub_dfies TYPE ddfields,
          ls_dfies     TYPE dfies.

    IF ir_structdescr->is_ddic_type( ) = abap_true.
      rt_dfies = ir_structdescr->get_ddic_field_list( p_langu = iv_language ).
      transform( CHANGING ct_dfies = rt_dfies ).
    ELSE.

      DATA(lt_components) = ir_structdescr->get_components( ).

      LOOP AT lt_components INTO ls_component.

        IF ls_component-as_include = abap_true.

          lr_struct ?= ls_component-type.
          lt_sub_dfies = read_structdescr( lr_struct ).

          LOOP AT lt_sub_dfies INTO ls_dfies.
            ls_dfies-fieldname = |{ ls_dfies-fieldname }{ ls_component-suffix }|.
            IF NOT ls_dfies-precfield IS INITIAL.
              ls_dfies-precfield = |{ ls_dfies-precfield }{ ls_component-suffix }|.
            ENDIF.
            APPEND ls_dfies TO rt_dfies.
          ENDLOOP.

        ELSE.

          CASE ls_component-type->kind.

            WHEN 'E'.
              lr_element ?= ls_component-type.
              IF lr_element->is_ddic_type( ) = abap_true.
                ls_dfies = lr_element->get_ddic_field( sy-langu ).
                transform( CHANGING cs_dfies = ls_dfies ).
                ls_dfies-fieldname = ls_component-name.
                APPEND ls_dfies TO rt_dfies.
              ELSE.
                CLEAR ls_dfies.
                ls_dfies-fieldname = ls_component-name.
                ls_dfies-inttype = lr_element->type_kind.
                ls_dfies-leng = lr_element->length.
                ls_dfies-decimals = lr_element->decimals.
                ls_dfies-convexit = lr_element->edit_mask.
                ls_dfies-outputlen = lr_element->output_length.
                ls_dfies-sign = 'X'.
                APPEND ls_dfies TO rt_dfies.
              ENDIF.

            WHEN 'S'.
              CONTINUE.
              lr_struct ?= ls_component-type.
              lt_sub_dfies = read_structdescr( lr_struct ).

              LOOP AT lt_sub_dfies INTO ls_dfies.
                ls_dfies-fieldname = |{ ls_component-name }-{ ls_dfies-fieldname }|.
                ls_dfies-precfield = |{ ls_component-name }-{ ls_dfies-precfield }|.
                IF NOT ls_dfies-precfield IS INITIAL.
                  ls_dfies-precfield = |{ ls_dfies-precfield }{ ls_component-suffix }|.
                ENDIF.
                APPEND ls_dfies TO rt_dfies.
              ENDLOOP.

          ENDCASE.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD transform.

    IF cs_dfies-tabname EQ cs_dfies-rollname OR
       cs_dfies-tabname EQ cs_dfies-domname.
      CLEAR cs_dfies-tabname.
    ENDIF.

    cs_dfies-reftable = cs_dfies-tabname.
    cs_dfies-reffield = cs_dfies-fieldname.
    CLEAR cs_dfies-precfield.

    LOOP AT ct_dfies ASSIGNING FIELD-SYMBOL(<ls_dfies>).

      CLEAR <ls_dfies>-precfield.
      IF <ls_dfies>-datatype = 'CURR' OR <ls_dfies>-datatype = 'QUAN'.

        IF line_exists( ct_dfies[ fieldname = <ls_dfies>-reffield ] ).
          <ls_dfies>-precfield = <ls_dfies>-reffield.
        ENDIF.

      ENDIF.

      <ls_dfies>-reftable = <ls_dfies>-tabname.
      <ls_dfies>-reffield = <ls_dfies>-fieldname.

    ENDLOOP.

    DELETE ct_dfies WHERE datatype = 'NODE'.

  ENDMETHOD.
ENDCLASS.
