unit comports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

procedure DetectComPorts(s: TStrings);

implementation

{$ifdef Windows}
uses windows, registry;

type
  HDEVINFO= Pointer;
   SP_DEVINFO_DATA = record
      cbSize: DWord;
      Guid: TGUID;
      DevInst: DWord;
      Reserve: DWord;
    end;
    PSP_DEVINFO_DATA = ^SP_DEVINFO_DATA;

   SP_DEVICE_INTERFACE_DETAIL_DATA = packed record
      cbSize:DWORD  ;
      DevicePath: array [0..0] of AnsiChar;
   end;
   PSP_DEVICE_INTERFACE_DETAIL_DATA  = ^SP_DEVICE_INTERFACE_DETAIL_DATA;

   SP_DEVICE_INTERFACE_DATA = record
      cbSize: DWORD  ;
      InterfaceClassGuid:TGUID  ;
      Flags:DWORD;
      Reserved:Pointer;
  end;
  PSP_DEVICE_INTERFACE_DATA  = ^SP_DEVICE_INTERFACE_DATA;

function SetupDiClassGuidsFromNameA(
                    ClassName: PCHAR;
                    ClassGuidList       : PGuid;
                    ClassGuidListSize    : DWORD;
                    RequiredSize         : PDWORD
                    ): Bool; stdcall; external 'setupapi.dll';

function SetupDiGetClassDevsA(
                    ClassGuid: PGUID;
                    Enumerator: PCHAR;
                    hwndParent: HWND;
                    Flags: DWORD
                    ): HDEVINFO; stdcall; external 'setupapi.dll';

function SetupDiEnumDeviceInfo(
                   DeviceInfoSet:HDEVINFO;
                   numDev:DWord;
                   DeviceInfoData: PSP_DEVINFO_DATA
                   ): Bool; stdcall; external 'setupapi.dll';

function  SetupDiGetDeviceRegistryPropertyA(
                   DeviceInfoSet:HDEVINFO;
                   DeviceInfoData:PSP_DEVINFO_DATA;
                   Propertys:DWORD;
                   PropertyRegDataType:PDWORD;
                   PropertyBuffer:PBYTE;
                   PropertyBufferSize:DWORD;
                   RequiredSize:PDWORD
                   ): Bool; stdcall; external 'setupapi.dll';

function SetupDiGetDeviceInterfaceDetailA(
                   DeviceInfoSet: HDEVINFO;
                   DeviceInterfaceData:PSP_DEVICE_INTERFACE_DATA;
                   DeviceInterfaceDetailData:PSP_DEVICE_INTERFACE_DETAIL_DATA;
                   DeviceInterfaceDetailDataSize: DWORD;
                   RequiredSize:PDWORD;
                   DeviceInfoData:PSP_DEVINFO_DATA
                   ): Bool; stdcall; external 'setupapi.dll';

function SetupDiEnumDeviceInterfaces(
                   DeviceInfoSet:HDEVINFO;
                   DeviceInfoData: PSP_DEVINFO_DATA;
                   InterfaceClassGuid:PGUID;
                   MemberIndex:DWORD;
                   DeviceInterfaceData: PSP_DEVICE_INTERFACE_DATA
                   ): Bool; stdcall; external 'setupapi.dll';

const
 DIGCF_DEFAULT                             = DWORD($00000001);
 DIGCF_PRESENT                             = DWORD($00000002);
 DIGCF_ALLCLASSES                          = DWORD($00000004);
 DIGCF_PROFILE                             = DWORD($00000008);
 DIGCF_DEVICEINTERFACE                    = DWORD($00000010);
 SPDRP_HARDWAREID                          = DWORD($00000001);
 SPDRP_FRIENDLYNAME                        = DWORD($0000000C);

procedure DetectComPorts(s: TStrings);
var guidTest,res1:Boolean;
    buf:PGuid;
    MemberIndex,RequiredSize:DWord;
    DeviceInfoSet:HDEVINFO;
    DeviceInterfaceData:SP_DEVICE_INTERFACE_DATA;
    DEVICE_INTERFACE_DETAIL_DATA:PSP_DEVICE_INTERFACE_DETAIL_DATA;
    reg: TRegistry;
    Key, ComPortName:String;
    DevicePath: pchar;
begin
   RequiredSize:=0;

   //**********************************
   //Retrive ClassGuid for Serial Ports from registry
   //**********************************
   guidTest:=SetupDiClassGuidsFromNameA('Ports',nil,0,@RequiredSize);
   if(RequiredSize < 1) then Exit;
   buf:=AllocMem(RequiredSize*sizeof(TGUID));
   guidTest:=SetupDiClassGuidsFromNameA('Ports',buf,RequiredSize*sizeof(TGUID),@RequiredSize);

   //**********************************
   //Retrive DeviceInfoSet to be used for enumeration
   //**********************************
   DeviceInfoSet:=SetupDiGetClassDevsA(buf,nil,0,(DIGCF_PRESENT or DIGCF_DEVICEINTERFACE));

   //**********************************
   //Enumerate all available ports
   //**********************************
   DeviceInterfaceData.cbSize:=sizeof(SP_DEVICE_INTERFACE_DATA);
   MemberIndex:=0;
   while(true) do
   begin
      res1:=SetupDiEnumDeviceInterfaces(DeviceInfoSet,nil,buf,MemberIndex,@DeviceInterfaceData);
      if (not res1) then
         break;

      //*************************************************
      //Determine the required size of the buffer and allocate the memory
      //*************************************************
      res1:=SetupDiGetDeviceInterfaceDetailA(DeviceInfoSet,@ DeviceInterfaceData,nil,0,@RequiredSize,nil);

      DEVICE_INTERFACE_DETAIL_DATA:=AllocMem(RequiredSize+sizeof(SP_DEVICE_INTERFACE_DETAIL_DATA));
      DEVICE_INTERFACE_DETAIL_DATA^.cbSize:=sizeof(SP_DEVICE_INTERFACE_DETAIL_DATA);

      res1:=SetupDiGetDeviceInterfaceDetailA(DeviceInfoSet,@ DeviceInterfaceData,DEVICE_INTERFACE_DETAIL_DATA,RequiredSize,@RequiredSize,nil);
      //********************************
      //Get the Devicepath to be used with createfile to get a handle for this interface
      // Devicepath works more reliable than using the comportnumber like 'com8'.
      // Even if two devices using the same comportnumber you are able to get a handle when using the devicepath.
      // if the devicepath starts with "\\?\usb" it is probably a USB2Serial adapter.
      //************************************************
      DevicePath:=PChar(@DEVICE_INTERFACE_DETAIL_DATA^.DevicePath);

      //**************************************
      // To get the Comportnumber who have to open the registry entry related to the device path
      // However this is not working very reliable because some vendors are using the # sign in there keynames. And the # sign is also used to separate the subkey names.
      //*********************************************
      reg:=TRegistry.Create();
      reg.RootKey:=HKEY_LOCAL_MACHINE;
      Key:=PChar(@DEVICE_INTERFACE_DETAIL_DATA^.DevicePath);
      Key:=StringReplace(Key,'#','\',[rfReplaceAll]);
      Key:=copy(Key,0,Pos('{',Key)-2);
      Key:='System\CurrentControlSet\Enum\'+copy(Key,5,RequiredSize)+'\Device Parameters';
      res1:=reg.OpenKeyReadOnly(Key);
      ComPortName:=reg.ReadString('PortName');
      reg.Free;

      s.add(ComPortName);

      inc(MemberIndex);
   end;
end;
{$else}
procedure DetectComPorts(s: TStrings);
begin

end;
{$endif}

end.

