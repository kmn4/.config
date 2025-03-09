#!/usr/bin/env fish

# copy-paste from franciscolourenco/done
function __done_run_powershell_script
    set -l powershell_exe (command --search "powershell.exe")

    if test $status -ne 0
        set powershell_exe "$HOME/win/System32/WindowsPowerShell/v1.0/powershell.exe"
    end

    if string length --quiet "$powershell_exe"
        and test -x "$powershell_exe"

        set cmd (string escape $argv)

        eval "$powershell_exe -Command $cmd"
    else
        echo notification failed.
    end
end

function __done_windows_notification -a title -a message
    __done_run_powershell_script "
[Windows.Data.Xml.Dom.XmlDocument, Windows.Data.Xml.Dom.XmlDocument, ContentType = WindowsRuntime] | Out-Null
[Windows.UI.Notifications.ToastNotification, Windows.UI.Notifications, ContentType = WindowsRuntime] | Out-Null

\$toast_xml_source = @\"
    <toast>
        $soundopt
        <visual>
            <binding template=\"ToastText02\">
                <text id=\"1\">$title</text>
                <text id=\"2\">$message</text>
            </binding>
        </visual>
    </toast>
\"@

\$toast_xml = New-Object Windows.Data.Xml.Dom.XmlDocument
\$toast_xml.loadXml(\$toast_xml_source)

\$toast = New-Object Windows.UI.Notifications.ToastNotification \$toast_xml

[Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier(\"fish\").Show(\$toast)
"
end

mail -e && __done_windows_notification 'メール' '未読のメールがあります'
