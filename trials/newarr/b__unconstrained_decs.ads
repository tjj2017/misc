pragma Ada_95;
pragma Warnings (Off);
with System;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: GPL 2016 (20160515-49)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_unconstrained_decs" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#387c5f71#;
   pragma Export (C, u00001, "unconstrained_decsB");
   u00002 : constant Version_32 := 16#b6df930e#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#937076cc#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#6326c08a#;
   pragma Export (C, u00004, "systemS");
   u00005 : constant Version_32 := 16#a6359005#;
   pragma Export (C, u00005, "system__memoryB");
   u00006 : constant Version_32 := 16#3a5ba6be#;
   pragma Export (C, u00006, "system__memoryS");
   u00007 : constant Version_32 := 16#e7214354#;
   pragma Export (C, u00007, "ada__exceptionsB");
   u00008 : constant Version_32 := 16#020f9e08#;
   pragma Export (C, u00008, "ada__exceptionsS");
   u00009 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00009, "adaS");
   u00010 : constant Version_32 := 16#e947e6a9#;
   pragma Export (C, u00010, "ada__exceptions__last_chance_handlerB");
   u00011 : constant Version_32 := 16#41e5552e#;
   pragma Export (C, u00011, "ada__exceptions__last_chance_handlerS");
   u00012 : constant Version_32 := 16#5f84b5ab#;
   pragma Export (C, u00012, "system__soft_linksB");
   u00013 : constant Version_32 := 16#fda218df#;
   pragma Export (C, u00013, "system__soft_linksS");
   u00014 : constant Version_32 := 16#b01dad17#;
   pragma Export (C, u00014, "system__parametersB");
   u00015 : constant Version_32 := 16#1d0ccdf5#;
   pragma Export (C, u00015, "system__parametersS");
   u00016 : constant Version_32 := 16#0f0cb66d#;
   pragma Export (C, u00016, "system__secondary_stackB");
   u00017 : constant Version_32 := 16#c8470fe3#;
   pragma Export (C, u00017, "system__secondary_stackS");
   u00018 : constant Version_32 := 16#39a03df9#;
   pragma Export (C, u00018, "system__storage_elementsB");
   u00019 : constant Version_32 := 16#4ee58a8e#;
   pragma Export (C, u00019, "system__storage_elementsS");
   u00020 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00020, "system__stack_checkingB");
   u00021 : constant Version_32 := 16#ed99ab62#;
   pragma Export (C, u00021, "system__stack_checkingS");
   u00022 : constant Version_32 := 16#87a448ff#;
   pragma Export (C, u00022, "system__exception_tableB");
   u00023 : constant Version_32 := 16#3e88a9c8#;
   pragma Export (C, u00023, "system__exception_tableS");
   u00024 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00024, "system__exceptionsB");
   u00025 : constant Version_32 := 16#0b45ad7c#;
   pragma Export (C, u00025, "system__exceptionsS");
   u00026 : constant Version_32 := 16#4c9e814d#;
   pragma Export (C, u00026, "system__exceptions__machineS");
   u00027 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00027, "system__exceptions_debugB");
   u00028 : constant Version_32 := 16#1dac394e#;
   pragma Export (C, u00028, "system__exceptions_debugS");
   u00029 : constant Version_32 := 16#6c2f8802#;
   pragma Export (C, u00029, "system__img_intB");
   u00030 : constant Version_32 := 16#61fd2048#;
   pragma Export (C, u00030, "system__img_intS");
   u00031 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00031, "system__tracebackB");
   u00032 : constant Version_32 := 16#3d041e4e#;
   pragma Export (C, u00032, "system__tracebackS");
   u00033 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00033, "system__traceback_entriesB");
   u00034 : constant Version_32 := 16#637d36fa#;
   pragma Export (C, u00034, "system__traceback_entriesS");
   u00035 : constant Version_32 := 16#0162f862#;
   pragma Export (C, u00035, "system__traceback__symbolicB");
   u00036 : constant Version_32 := 16#dd19f67a#;
   pragma Export (C, u00036, "system__traceback__symbolicS");
   u00037 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00037, "ada__exceptions__tracebackB");
   u00038 : constant Version_32 := 16#20245e75#;
   pragma Export (C, u00038, "ada__exceptions__tracebackS");
   u00039 : constant Version_32 := 16#5ab55268#;
   pragma Export (C, u00039, "interfacesS");
   u00040 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00040, "interfaces__cB");
   u00041 : constant Version_32 := 16#70be4e8c#;
   pragma Export (C, u00041, "interfaces__cS");
   u00042 : constant Version_32 := 16#5f72f755#;
   pragma Export (C, u00042, "system__address_operationsB");
   u00043 : constant Version_32 := 16#702a7eb9#;
   pragma Export (C, u00043, "system__address_operationsS");
   u00044 : constant Version_32 := 16#13b71684#;
   pragma Export (C, u00044, "system__crtlS");
   u00045 : constant Version_32 := 16#f82008fb#;
   pragma Export (C, u00045, "system__dwarf_linesB");
   u00046 : constant Version_32 := 16#0aa7ccc7#;
   pragma Export (C, u00046, "system__dwarf_linesS");
   u00047 : constant Version_32 := 16#12c24a43#;
   pragma Export (C, u00047, "ada__charactersS");
   u00048 : constant Version_32 := 16#8f637df8#;
   pragma Export (C, u00048, "ada__characters__handlingB");
   u00049 : constant Version_32 := 16#3b3f6154#;
   pragma Export (C, u00049, "ada__characters__handlingS");
   u00050 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00050, "ada__characters__latin_1S");
   u00051 : constant Version_32 := 16#af50e98f#;
   pragma Export (C, u00051, "ada__stringsS");
   u00052 : constant Version_32 := 16#e2ea8656#;
   pragma Export (C, u00052, "ada__strings__mapsB");
   u00053 : constant Version_32 := 16#1e526bec#;
   pragma Export (C, u00053, "ada__strings__mapsS");
   u00054 : constant Version_32 := 16#04ec3c16#;
   pragma Export (C, u00054, "system__bit_opsB");
   u00055 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00055, "system__bit_opsS");
   u00056 : constant Version_32 := 16#57a0bc09#;
   pragma Export (C, u00056, "system__unsigned_typesS");
   u00057 : constant Version_32 := 16#92f05f13#;
   pragma Export (C, u00057, "ada__strings__maps__constantsS");
   u00058 : constant Version_32 := 16#57a37a42#;
   pragma Export (C, u00058, "system__address_imageB");
   u00059 : constant Version_32 := 16#c2ca5db0#;
   pragma Export (C, u00059, "system__address_imageS");
   u00060 : constant Version_32 := 16#ec78c2bf#;
   pragma Export (C, u00060, "system__img_unsB");
   u00061 : constant Version_32 := 16#c85480fe#;
   pragma Export (C, u00061, "system__img_unsS");
   u00062 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00062, "system__ioB");
   u00063 : constant Version_32 := 16#fd6437c5#;
   pragma Export (C, u00063, "system__ioS");
   u00064 : constant Version_32 := 16#cf909744#;
   pragma Export (C, u00064, "system__object_readerB");
   u00065 : constant Version_32 := 16#27c18a1d#;
   pragma Export (C, u00065, "system__object_readerS");
   u00066 : constant Version_32 := 16#1a74a354#;
   pragma Export (C, u00066, "system__val_lliB");
   u00067 : constant Version_32 := 16#f902262a#;
   pragma Export (C, u00067, "system__val_lliS");
   u00068 : constant Version_32 := 16#afdbf393#;
   pragma Export (C, u00068, "system__val_lluB");
   u00069 : constant Version_32 := 16#2d52eb7b#;
   pragma Export (C, u00069, "system__val_lluS");
   u00070 : constant Version_32 := 16#27b600b2#;
   pragma Export (C, u00070, "system__val_utilB");
   u00071 : constant Version_32 := 16#cf867674#;
   pragma Export (C, u00071, "system__val_utilS");
   u00072 : constant Version_32 := 16#d1060688#;
   pragma Export (C, u00072, "system__case_utilB");
   u00073 : constant Version_32 := 16#472fa95d#;
   pragma Export (C, u00073, "system__case_utilS");
   u00074 : constant Version_32 := 16#84a27f0d#;
   pragma Export (C, u00074, "interfaces__c_streamsB");
   u00075 : constant Version_32 := 16#b1330297#;
   pragma Export (C, u00075, "interfaces__c_streamsS");
   u00076 : constant Version_32 := 16#931ff6be#;
   pragma Export (C, u00076, "system__exception_tracesB");
   u00077 : constant Version_32 := 16#47f9e010#;
   pragma Export (C, u00077, "system__exception_tracesS");
   u00078 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00078, "system__wch_conB");
   u00079 : constant Version_32 := 16#785be258#;
   pragma Export (C, u00079, "system__wch_conS");
   u00080 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00080, "system__wch_stwB");
   u00081 : constant Version_32 := 16#554ace59#;
   pragma Export (C, u00081, "system__wch_stwS");
   u00082 : constant Version_32 := 16#a831679c#;
   pragma Export (C, u00082, "system__wch_cnvB");
   u00083 : constant Version_32 := 16#77ec58ab#;
   pragma Export (C, u00083, "system__wch_cnvS");
   u00084 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00084, "system__wch_jisB");
   u00085 : constant Version_32 := 16#f79c418a#;
   pragma Export (C, u00085, "system__wch_jisS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.handling%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.case_util%s
   --  system.case_util%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.io%s
   --  system.io%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  system.soft_links%s
   --  system.unsigned_types%s
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.val_lli%s
   --  system.val_llu%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%b
   --  system.val_lli%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  system.address_image%s
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.strings%s
   --  ada.strings.maps%s
   --  ada.strings.maps.constants%s
   --  interfaces.c%s
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.exception_traces%s
   --  system.exception_traces%b
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.object_reader%s
   --  system.dwarf_lines%s
   --  system.secondary_stack%s
   --  interfaces.c%b
   --  ada.strings.maps%b
   --  system.soft_links%b
   --  ada.characters.handling%b
   --  system.secondary_stack%b
   --  system.dwarf_lines%b
   --  system.object_reader%b
   --  system.address_image%b
   --  ada.exceptions.traceback%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  unconstrained_decs%b
   --  END ELABORATION ORDER


end ada_main;
