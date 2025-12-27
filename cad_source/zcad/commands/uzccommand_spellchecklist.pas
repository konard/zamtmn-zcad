{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  See the file COPYING.txt, included in this distribution,                 *
*  for details about the copyright.                                         *
*                                                                           *
*  This program is distributed in the hope that it will be useful,          *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
*                                                                           *
*****************************************************************************
}
{
@author(Vladimir Bobrov)
}
{$mode objfpc}{$H+}
{$INCLUDE zengineconfig.inc}

unit uzcCommand_SpellCheckList;

interface

uses
  uzcLog,
  uzccommandsabstract, uzccommandsimpl,
  uzcfspellchecker,
  uzcdrawings;

implementation

// Команда для отображения формы проверки орфографии
function SpellCheckList_com(const Context: TZCADCommandContext;
  operands: TCommandOperands): TCommandResult;
begin
  if drawings.GetCurrentDWG <> nil then
  begin
    programlog.LogOutStr(
      'uzccommand_spellchecklist: opening spell checker form',
      LM_Info
    );
    ShowSpellCheckerForm;
    Result := cmd_ok;
  end
  else
  begin
    programlog.LogOutStr(
      'uzccommand_spellchecklist: no current drawing',
      LM_Info
    );
    Result := cmd_error;
  end;
end;

initialization
  programlog.LogOutFormatStr(
    'Unit "%s" initialization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsInitializeLMId
  );
  CreateZCADCommand(@SpellCheckList_com, 'SpellCheckList', 0, 0);

finalization
  ProgramLog.LogOutFormatStr(
    'Unit "%s" finalization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsFinalizeLMId
  );

end.
