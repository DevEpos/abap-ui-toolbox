"! <p class="shorttext synchronized" lang="en">UI Command Handler</p>
INTERFACE zif_uitb_gui_command_handler
  PUBLIC .

  "! <p class="shorttext synchronized" lang="en">Execute the given command</p>
  "!
  METHODS execute_command
    IMPORTING
      io_command type ref to zif_uitb_gui_command.
ENDINTERFACE.
