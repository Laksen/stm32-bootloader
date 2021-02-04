unit elfobj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

type
 TElf32_Addr   = longword;
 TElf32_Half   = word;
 TElf32_Off    = longword;
 TElf32_Sword  = longint;
 TElf32_Word   = longword;

 TElf32header=packed record
  magic0123         : longint;
  file_class        : byte;
  data_encoding     : byte;
  file_version      : byte;
  padding           : array[$07..$0f] of byte;
  e_type            : TElf32_Half;
  e_machine         : TElf32_Half;
  e_version         : TElf32_Word;
  e_entry           : TElf32_Addr;          { entrypoint }
  e_phoff           : TElf32_Off;           { program header offset }
  e_shoff           : TElf32_Off;           { sections header offset }
  e_flags           : TElf32_Word;
  e_ehsize          : TElf32_Half;             { elf header size in bytes }
  e_phentsize       : TElf32_Half;             { size of an entry in the program header array }
  e_phnum           : TElf32_Half;             { 0..e_phnum-1 of entrys }
  e_shentsize       : TElf32_Half;             { size of an entry in sections header array }
  e_shnum           : TElf32_Half;             { 0..e_shnum-1 of entrys }
  e_shstrndx        : TElf32_Half;             { index of string section header }
 end;

 TElf32phdr=packed record
  p_type           : TElf32_Word;
  p_offset         : TElf32_Off;
  p_vaddr          : TElf32_Addr;
  p_paddr          : TElf32_Addr;
  p_filesz         : TElf32_Word;
  p_memsz          : TElf32_Word;
  p_flags          : TElf32_Word;
  p_align          : TElf32_Word;
 end;

 { TElfObject }

 TElfObject = class
 private
  fs: TStream;
  fHeader: TElf32header;
  fPHeaders: array of TElf32phdr;
  function GetSectionAddress(AIndex: longint): longint;
  function GetSectionCount: longint;
  function GetSectionSize(AIndex: longint): longint;
 public
  function ReadSection(AIndex, AOffset: longint; var Buffer; BufferSize: longint): longint;

  constructor Create(S: TStream);
  destructor Destroy; override;

  property SectionCount: longint read GetSectionCount;
  property SectionSize[AIndex: longint]: longint read GetSectionSize;
  property SectionAddress[AIndex: longint]: longint read GetSectionAddress;
 end;

implementation

const
 ElfMagic = $464C457F;

 ET_EXEC = 2;

 EM_ARM = $28;

{ TElfObject }

function TElfObject.GetSectionAddress(AIndex: longint): longint;
begin
   result := fPHeaders[AIndex].p_paddr;
end;

function TElfObject.GetSectionCount: longint;
begin
   result := length(fPHeaders);
end;

function TElfObject.GetSectionSize(AIndex: longint): longint;
begin
   result := fPHeaders[AIndex].p_filesz;
end;

function TElfObject.ReadSection(AIndex, AOffset: longint; var Buffer; BufferSize: longint): longint;
begin
   fs.Seek(fPHeaders[AIndex].p_offset+AOffset, soBeginning);
   result := min(fPHeaders[AIndex].p_filesz-AOffset, BufferSize);
   fs.ReadBuffer(buffer, result);
end;

constructor TElfObject.Create(S: TStream);
begin
   inherited Create;
   fs := s;
   s.Seek(0, soBeginning);
   s.ReadBuffer(fHeader, sizeof(TElf32header));
   setlength(fPHeaders, fHeader.e_phnum);
   s.Seek(fHeader.e_phoff, soBeginning);
   s.ReadBuffer(fPHeaders[0], length(fPHeaders)*SizeOf(TElf32phdr));

   // Validate
   if fHeader.magic0123 <> ElfMagic then raise exception.Create('Wrong ELF magic');
   if fHeader.e_type <> ET_EXEC then raise exception.Create('Wrong ELF type. Expected executable object');
   if fHeader.e_machine <> EM_ARM then raise exception.Create('Wrong ELF machine. Expected ARM');
end;

destructor TElfObject.Destroy;
begin
   inherited Destroy;
end;

end.

