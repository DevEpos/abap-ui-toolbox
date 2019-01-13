CLASS zcl_uitb_generic_importer DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED .

  PUBLIC SECTION.

    METHODS import_data .
  PROTECTED SECTION.

    TYPES:
      x1024 TYPE x LENGTH 1024 .

    DATA:
      mt_xml_data TYPE STANDARD TABLE OF x1024 .
    DATA mr_import_data TYPE REF TO data .
    DATA mv_default_file_name TYPE string .
    DATA mr_log TYPE REF TO zcl_uitb_log .

    METHODS constructor
      IMPORTING
        !iv_default_file_name TYPE string .
    METHODS create_internal_table_ref
          ABSTRACT
      RETURNING
        VALUE(rr_table_ref) TYPE REF TO data .
    METHODS write_no_data_found_message
         ABSTRACT .
    METHODS filter_data .
    METHODS process_import_data
         ABSTRACT .
    METHODS persist_import_data
         ABSTRACT .
    METHODS write_start_message
         ABSTRACT .
  PRIVATE SECTION.

    METHODS create_data_from_xml.
    METHODS read_data_from_file.
ENDCLASS.



CLASS ZCL_UITB_GENERIC_IMPORTER IMPLEMENTATION.


  METHOD constructor.
    mv_default_file_name = COND #( WHEN iv_default_file_name IS INITIAL THEN
                                    'import'
                                   ELSE
                                     iv_default_file_name ).

    mr_log = NEW #( ).
  ENDMETHOD.


  METHOD create_data_from_xml.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    ASSIGN mr_import_data->* TO <lt_table>.

    DATA(lr_json_reader) = cl_sxml_table_reader=>create( input = mt_xml_data ).

    lr_json_reader->set_option( EXPORTING option = if_sxml_reader=>co_opt_normalizing value = abap_true ).

    CALL TRANSFORMATION id
                        SOURCE XML lr_json_reader
                        RESULT xmldat = <lt_table>.
  ENDMETHOD.


  METHOD filter_data.
    " redefine if necessary
  ENDMETHOD.


  METHOD import_data.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    write_start_message( ).

    TRY .
        read_data_from_file( ).

        mr_import_data = create_internal_table_ref( ).

        ASSIGN mr_import_data->* TO <lt_table>.

        create_data_from_xml( ).

        filter_data( ).

        IF <lt_table> IS INITIAL.
          write_no_data_found_message( ).
          RETURN.
        ENDIF.

        process_import_data( ).

        persist_import_data( ).
      CATCH zcx_uitb_exception INTO DATA(lr_exception).
        lr_exception->print_message( if_to_screen = abap_false ).
        mr_log->add_from_sy( if_newobj = abap_false
                             iv_level  = zif_uitb_c_protocol_level=>error ).
    ENDTRY.

    mr_log->protocol_write( ).
  ENDMETHOD.


  METHOD read_data_from_file.
    DATA: lv_filename      TYPE          string,
          lv_fullpath      TYPE          string,
          lt_filenames     TYPE TABLE OF file_table,
          lv_download_path TYPE          string,
          lv_upload_path   TYPE          string,
          lv_useraction    TYPE          i,
          ln_filecount     TYPE          i.

    lv_filename = mv_default_file_name && '.XML'.

    cl_gui_frontend_services=>get_upload_download_path(
      CHANGING
        download_path = lv_download_path
        upload_path   = lv_upload_path
      EXCEPTIONS
        OTHERS        = 1
    ).

    IF sy-subrc <> 0.
      lv_download_path = lv_upload_path = 'C:\'.
    ENDIF.

    IF lv_download_path IS INITIAL.
      lv_download_path = 'C:\'.
    ENDIF.

    IF lv_upload_path IS INITIAL.
      lv_upload_path = 'C:\'.
    ENDIF.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        default_extension = '.XML'
        default_filename  = lv_filename
        initial_directory = lv_upload_path
        file_filter       = '*.XML'
      CHANGING
        file_table        = lt_filenames
        rc                = ln_filecount
        user_action       = lv_useraction
     ).

    IF lv_useraction = cl_gui_frontend_services=>action_ok AND
       ln_filecount = 1.

      lv_fullpath = lt_filenames[ 1 ].

      cl_gui_frontend_services=>gui_upload(
        EXPORTING
          filename = lv_fullpath
          filetype = 'BIN'
        CHANGING
          data_tab = mt_xml_data
       ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_uitb_exception
        EXPORTING
          textid = zcx_uitb_exception=>cancelled_by_user.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
