;;; init-powershell.el --- Windows powershell  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(defconst my:powershell-env-from-registry
  "\
$exclude = @('USERNAME', 'PROCESSOR_ARCHITECTURE', 'PATH', 'PSModulePath',
             'TERM', 'SHELL');
$sysreg = \
'HKLM:\\SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment';
$userreg = 'HKCU:\\Environment';
$sysvars = get-item $sysreg;
$uservars = get-item $userreg;

$sysexclude = $exclude + $uservars.GetValueNames();

$sysvars.GetValueNames() \
| ?{ $sysexclude -notcontains $_ } \
| %{ \"$($_)=$($sysvars.GetValue($_))\" } \
| %{ [System.Environment]::ExpandEnvironmentVariables($_) } \
| write-output;

$uservars.GetValueNames() \
| ?{ $exclude -notcontains $_ } \
| %{ \"$($_)=$($uservars.GetValue($_))\" } \
| %{ [System.Environment]::ExpandEnvironmentVariables($_) } \
| write-output;

$paths = \
 $sysvars.GetValue('PATH'), $uservars.GetValue('PATH') \
 | %{ $_ -split ';' } \
 | %{ [System.Environment]::ExpandEnvironmentVariables($_) } \
 | select -unique;
write-output \"PATH=$($paths -join ';')\"")


(defun my:poweshell-exec-command (command)
  "Execute powershell COMMAND synchronously.
Return output as string."
  (with-output-to-string
    (call-process-region command nil
                         "powershell" nil standard-output nil
                         "-noprofile" "-noninteractive" "-command" "-")))


(provide 'init-powershell)

;;; init-powershell.el ends here
