"! <p class="shorttext synchronized" lang="en">Holds version information</p>
CLASS zcl_uitb_version DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Current version of UI Toolbox</p>
    "! <strong>Changelog</strong> <br/><br/>
    "!
    "! ## v1.6.0    - 2019-06-27
    "!
    "! ### Features
    "!
    "! - Minor updates in event handling in ALV Grid Adapter
    "!
    "! ## v1.5.0    - 2019-06-03
    "!
    "! ### Features
    "!
    "! - Sort the shortcuts in the shortcut viewer popup
    "! - Add Quickfilter to context menu of ALV
    "!
    "! ### Fixes
    "!
    "! - Fix issue if ALV is opened in dialog/modal dialog mode
    "!
    "! ## v1.4.1    - 2019-05-20
    "!
    "! ### Fixes
    "!
    "! - mr_control of column tree model is now filled with tree control
    "! - set_visible method inside ALV to show/hide grid control
    "!
    "! ## v1.4.0    - 2019-05-10
    "!
    "! ### Features
    "!
    "! - New generic selection dialog
    "!
    "! ### Fixes
    "!
    "! - No longer create document in zcl_uitb_gui_code_editor
    "!
    CONSTANTS c_version TYPE string VALUE '1.6.0'.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_uitb_version IMPLEMENTATION.
ENDCLASS.
