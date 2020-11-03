;;; alert-toast.el --- Windows 10 toast notifications -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Grzegorz Kowzan

;; Author: Grzegorz Kowzan <grzegorz@kowzan.eu>
;; Created: 25 Oct 2020
;; Updated: 25 Oct 2020
;; Version: 0.1
;; Package-Requires: ((alert "1.2") (f "0.20.0") (s "1.12.0"))
;; Keywords: notification emacs message windows wsl
;; X-URL: https://github.com/gkowzan/alert-toast

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package defines a new alert style (`toast') for alert.el using Windows
;; 10 toast notifications. It works with native Windows 10 Emacs versions and
;; with Emacs run under Windows Subsystem for Linux (WSL) or under Cygwin. These
;; notifications are limited to a single-line title and four lines of text.
;; Longer text can be passed but it will be truncated by Windows 10.
;;
;; * Icons
;; Icons located on network shares are not supported. This includes icons on the
;; WSL virtual drive, therefore for Emacs running under WSL the default Emacs
;; icon is copied to C:\Users\<user>\AppData\Local\Emacs-Toast\Emacs.png. PNG
;; version is used because toast notifications render SVG graphics as tiny and
;; put them in top left corner of the notification.

;; Under WSL or Cygwin, a path to a custom icon should be given as a WSL/Cygwin
;; path (/mnt/c/... or /cygdrive/c/...) instead of a Windows path (C:\\...).
;;
;; * Priorities
;; Looking at Windows.UI.Notifications API, toast notifications seem to support
;; 2 priority levels: High and Default. Mapping between alert.el priorities and
;; these levels is defined by `alert-toast-priorities'.
;;
;; * Bugs
;; There is an issue in WSL where wslhost.exe dies for no discernible reason,
;; which prevents accessing Windows partitions and executables
;; (https://github.com/microsoft/WSL/issues/6161). If this happens then you
;; should see powershell.exe process failing to start. This will obviously
;; prevent this package from working. The only known workaround is to call `wsl
;; --shutdown' and start WSL again.
;;; Code:

(require 'f)
(require 's)
(require 'alert)

;; WSL-related functions and constants
;; In the words of WSL developers, there is no official way of testing for WSL
;; but they said at the same time that either "wsl" or "microsoft" should always
;; be present in the kernel release string.
(defun alert-toast--check-wsl ()
  (and (eq system-type 'gnu/linux)
       (let ((kernel-release (shell-command-to-string "uname --kernel-release")))
         (or (s-contains? "wsl" kernel-release t)
             (s-contains? "microsoft" kernel-release t)))))

(defconst alert-toast--wsl (alert-toast--check-wsl))
(defconst alert-toast--appdir-text "[System.Environment]::GetFolderPath([System.Environment+SpecialFolder]::LocalApplicationData) | Join-Path -ChildPath Emacs-Toast\\Emacs.png")

(defun alert-toast--appdir ()
  "Path to Windows user's data directory."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-dos))
      (call-process-region alert-toast--appdir-text nil "powershell.exe" nil t nil "-noprofile" "-NonInteractive" "-WindowStyle" "Hidden" "-Command" "-"))
    (s-chomp (buffer-string))))

(defun alert-toast--default-wsl-icon-path ()
  "Path to Emacs icon in Windows user's data directory."
  (with-temp-buffer
    (call-process "wslpath" nil t nil (alert-toast--appdir))
    (s-chomp (buffer-string))))

(defun alert-toast--init-wsl-icon ()
  "Copy Emacs icon to a Windows-side directory."
  (let ((icon-path (alert-toast--default-wsl-icon-path)))
    (unless (f-exists? icon-path)
      (make-directory (f-parent icon-path) t)
      (f-copy (concat data-directory "images/icons/hicolor/128x128/apps/emacs.png")
              icon-path))))

(defun alert-toast--icon-path (path)
  "Convert icon PATH from WSL/Cygwin to Windows path if needed."
  (cond
   (alert-toast--wsl
    (with-temp-buffer
      (call-process "wslpath" nil t nil "-m" path)
      (s-chomp (buffer-string))))
   ((eq system-type 'cygwin)
    (with-temp-buffer
      (call-process "cygpath.exe" nil t nil "-w" path)
      (s-chomp (buffer-string))))
   (t path)))

;; Default icon
(defvar alert-toast-default-icon
  (if alert-toast--wsl
      (alert-toast--default-wsl-icon-path)
    (concat data-directory "images/icons/hicolor/128x128/apps/emacs.png"))
  "Path to default icon for toast notifications.")

;; Common part -- script body, powershell quoting, priorities and main function
(defconst alert-toast--psquote-replacements
  '(("'" . "''")))

(defcustom alert-toast-priorities
  '((urgent . "[Windows.UI.Notifications.ToastNotificationPriority]::High")
    (high . "[Windows.UI.Notifications.ToastNotificationPriority]::High")
    (moderate . "[Windows.UI.Notifications.ToastNotificationPriority]::Default")
    (normal . "[Windows.UI.Notifications.ToastNotificationPriority]::Default")
    (low . "[Windows.UI.Notifications.ToastNotificationPriority]::Default")
    (trivial . "[Windows.UI.Notifications.ToastNotificationPriority]::Default"))
  "A mapping of alert severities onto Windows 10 toast priority values."
  :type '(alist :key-type symbol :value-type string)
  :group 'alert)

(defvar alert-toast--psprocess nil
  "Persistent powershell process emitting toast notifications.")

(defun alert-toast--coding-page ()
  "Get powershell encoding."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-dos))
      (call-process-region "[console]::InputEncoding.BodyName" nil "powershell.exe" nil t nil "-noprofile" "-NonInteractive" "-WindowStyle" "Hidden" "-Command" "-"))
    (intern-soft (s-chomp (buffer-string)))))

(defun alert-toast--psprocess-init ()
  "Initialize powershell process."
  (setq alert-toast--psprocess
        (make-process :name "powershell-toast"
                      :buffer "*powershell-toast*"
                      :command '("powershell.exe" "-noprofile" "-NoExit" "-NonInteractive" "-WindowStyle" "Hidden"
                                 "-Command" "-")
                      :coding (if alert-toast--wsl 'utf-8 (alert-toast--coding-page))
                      :noquery t
                      :connection-type 'pipe))
  (process-send-string alert-toast--psprocess "[Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType = WindowsRuntime] > $null
    $Template = [Windows.UI.Notifications.ToastNotificationManager]::GetTemplateContent([Windows.UI.Notifications.ToastTemplateType]::ToastImageAndText02)\n"))

(defconst alert-toast--psscript-text "$ToastTitle = '%s'
    $ToastText = '%s'

    $RawXml = [xml] $Template.GetXml()
    ($RawXml.toast.visual.binding.text|where {$_.id -eq \"1\"}).AppendChild($RawXml.CreateTextNode($ToastTitle)) > $null
    ($RawXml.toast.visual.binding.text|where {$_.id -eq \"2\"}).AppendChild($RawXml.CreateTextNode($ToastText)) > $null
    $RawXml.toast.visual.binding.image.src = 'file://%s'

    $SerializedXml = New-Object Windows.Data.Xml.Dom.XmlDocument
    $SerializedXml.LoadXml($RawXml.OuterXml)

    $Toast = [Windows.UI.Notifications.ToastNotification]::new($SerializedXml)
    $Toast.Tag = \"Emacs\"
    $Toast.Group = \"Emacs\"
    $Toast.Priority = %s
    $Toast.ExpirationTime = [DateTimeOffset]::Now.AddSeconds(%f)

    $Notifier = [Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier(\"Emacs\")
    $Notifier.Show($Toast);\n")

;;;###autoload
(defun alert-toast-notify (info)
  "Send INFO using Windows 10 toast notification.
Handles :ICON, :SEVERITY, :PERSISTENT, :NEVER-PERSIST, :TITLE and :MESSAGE keywords
from INFO plist."
  (let ((psscript (format alert-toast--psscript-text
                          (s-replace-all alert-toast--psquote-replacements (plist-get info :title))
                          (s-replace-all alert-toast--psquote-replacements (plist-get info :message))
                          (s-replace-all alert-toast--psquote-replacements
                                         (alert-toast--icon-path (or (plist-get info :icon)
                                                                     alert-toast-default-icon)))
                          (let ((priority (cdr (assq (plist-get info :severity) alert-toast-priorities))))
                            (if priority
                                priority
                              (cdr (assq 'normal alert-toast-priorities))))
                          (if (and (plist-get info :persistent)
                                   (not (plist-get info :never-persist)))
                              (* 60 60 24 7)  ; a week
                            alert-fade-time))))
    (unless alert-toast--psprocess
      (alert-toast--psprocess-init))
    (process-send-string alert-toast--psprocess psscript)))

(alert-define-style 'toast :title "Windows 10 toast notification"
                    :notifier #'alert-toast-notify)

(when alert-toast--wsl
  (alert-toast--init-wsl-icon))

(provide 'alert-toast)
;;; alert-toast.el ends here
;; (alert-toast-notify '(:title "Tytuł" :message "Zaźółć gęślą jaźń"))
