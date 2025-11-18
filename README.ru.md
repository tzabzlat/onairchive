# On Airchive
[![License](http://poser.pugx.org/tzabzlat/yii2-sentry/license)](https://packagist.org/packages/tzabzlat/yii2-sentry)

Демон для фоновой записи интернет-радио. Работает фоном, нарезает почасовые аудиофайлы и сохраняет названия передач.

*Read this in other languages: [English](README.md), [Русский](README.ru.md)*

---

### Ключевые функции

- **Автовосстановление** - автоматическое переподключение при сбоях связи
- **Параллельная запись** - возможность записывать несколько радиостанций параллельно
- **Детекция тишины** - переподключение при обнаружении тишины в потоке
- Можно установить в Windows как службу для автозапуска без пользователя
- Настраиваемая буферизация в ОЗУ для минимальной нагрузки на диск
- Кодирование в AAC через ffmpeg
- Автоматическое создание файлов с метками времени
- Ротация файлов по дате и времени
- Поддержка различных часовых поясов для каждого потока
- Логирование событий

---

## Как пользоваться

### Windows GUI

Графическое приложение для Windows с визуальным интерфейсом.

**Запуск:**
1. Скачайте zip-архив сборки для Windows
2. Распакуйте в удобную папку
3. Отредактируйте `config.ini` - укажите URL-адреса ваших потоков
4. Запустите `OnAirchiveVCL.exe`
5. Наблюдайте за записью в окне приложения

**В сборке для Windows:**
- Включены библиотеки BASS в `Libs/Windows/x86/` (bass.dll, bassenc.dll, bass_aac.dll)
- Включен предварительно собранный ffmpeg.exe (32-bit) от [sudo-nautilus](https://github.com/sudo-nautilus/FFmpeg-Builds-Win32/releases/download/latest/ffmpeg-n5.1-latest-win32-gpl-5.1.zip)

### Windows Service

Фоновая служба для непрерывной работы на серверах Windows.

**Установка и запуск службы:**

Запустите консоль от имени администратора:

```cmd
; Установите службу
OnAirchiveSVC.exe /install

:: Запустите службу
net start OnAirchiveSVCService
```

**Управление через GUI:**

1. Нажмите `Win + R`
2. Введите `services.msc` и нажмите Enter
3. Найдите **OnAirchiveSVCService** в списке служб
4. Управляйте службой через контекстное меню (Пуск/Стоп/Перезапуск)

**Дополнительные команды:**

```cmd
; Остановить
net stop OnAirchiveSVCService

; Удалить службу
OnAirchiveSVC.exe /uninstall
```

### Linux Console

Консольное приложение для Linux-серверов.

**Шаг 1: Установите пакет ffmpeg**

```bash
apt-get install ffmpeg
```

**Шаг 2: Запуск через скрипт**

```bash
./run.sh
```

**Важные примечания для Linux:**
- В репозитории включены библиотеки BASS для Linux в `Libs/Linux/x86_64/` (libbass.so, libbassenc.so, libbass_aac.so)
- Библиотеки должны находиться в той же папке, что и исполняемый файл

**Пример конфигурации (config.ini):**

```ini
; General settings
[General]
; RAM buffer size to reduce disk access frequency
DataBufferSizeKb=5120

; Root directory for recordings
[Storage]
FilesPath=MyRadioArchive
; FilesPath=D:\MyRadioArchive
; FilesPath=/home/user/archive

; Radio station stream settings
[Stream]
; Radio station stream URL in mp3 or aac format
StreamUri0=https://am.radioaurora.am/al.mp3
; Subfolder with this name will be created in FilesPath directory
StreamToken0=Aurora
; Radio station timezone for correct timestamps
StreamTz0=Asia/Yerevan
```

---

## Результат

Программа автоматически организует записи по часам:

```
root/ (FilesPath)
└── Aurora/
    └── 2020-11-23/
        ├── [2020-11-23 00-00-00] Aurora.aac
        ├── [2020-11-23 00-00-00] Aurora.txt
        ├── [2020-11-23 01-00-00] Aurora.aac
        ├── [2020-11-23 01-00-00] Aurora.txt
        ├── [2020-11-23 02-00-00] Aurora.aac
        ├── [2020-11-23 02-00-00] Aurora.txt
        └── ...
```

**Форматы файлов:**
- **`*.aac`** - файл с аудио одного часа (в часовом поясе StreamTz)
- **`*.txt`** - список передач/треков текущего часа (если станция передавала теги)

## Как собрать

### Поддерживаемые платформы и инструменты

| Платформа | Цель сборки | Инструмент | Выходной файл |
|-----------|-------------|------------|---------------|
| **Windows** | GUI (Desktop) | Delphi 10.2+ / RAD Studio | OnAirchiveVCL.exe |
| **Windows** | Service (Служба) | Delphi 10.2+ / RAD Studio | OnAirchiveSVC.exe |
| **Linux** | Console (Демон) | Free Pascal 3.2+ | onairchive |

### Сборка для Windows

#### Требования:
- **Embarcadero Delphi** 10.2 Tokyo или выше (или RAD Studio)
- **Windows** 7 SP1 или выше
- Библиотеки BASS уже включены в репозиторий

#### Сборка через Delphi IDE:

**Windows GUI:**
1. Откройте `OnAirchive.groupproj` в RAD Studio
2. Выберите **Project → Build OnAirchiveVCL**
3. Выберите конфигурацию **Release**
4. Результат: `Output/Windows/Release/OnAirchiveVCL.exe`

**Windows Service:**
1. Откройте `OnAirchive.groupproj` в RAD Studio
2. Выберите **Project → Build OnAirchiveSVC**
3. Выберите конфигурацию **Release**
4. Результат: `Output/Windows/Release/OnAirchiveSVC.exe`

### Сборка для Linux

#### Требования:
- **Free Pascal Compiler (FPC)** 3.2.0 или выше
- **ffmpeg** (устанавливается через менеджер пакетов)
- Библиотеки BASS для Linux (уже включены в репозиторий)

#### Установка зависимостей:

```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install fpc ffmpeg
```

#### Сборка:

**Вариант 1: Скрипт сборки (рекомендуется)**

```bash
cd Platforms/Linux/
chmod +x build.sh
./build.sh
```

Результат: `Output/Linux/onairchive`
Запустить через `./run.sh`

**Примечание:** В репозитории уже присутствуют библиотеки BASS для Linux в `Libs/Linux/x86_64/` (libbass.so, libbassenc.so, libbass_aac.so). Они автоматически копируются в выходную папку при сборке через build.sh.

---

## Используемые технологии

### BASS Audio Library

OnAirchive использует [BASS Audio Library](https://www.un4seen.com/) для работы с аудиопотоками:

- **BASS** - основная библиотека для потоковой передачи и декодирования
- **BASSENC** - декодирование аудио
- **BASS_AAC** - плагин для поддержки AAC

**Лицензия:** BASS бесплатна для некоммерческого использования. Для коммерческого использования требуется лицензия.

**Ссылки:**
- Основной сайт: https://www.un4seen.com/
- Документация: https://www.un4seen.com/doc/

### FFmpeg

Используется для кодирования аудио:

- **Windows:** Включен предварительно собранный ffmpeg.exe из [sudo-nautilus/FFmpeg-Builds-Win32](https://github.com/sudo-nautilus/FFmpeg-Builds-Win32/releases)
- **Linux:** Устанавливается через менеджер пакетов (`apt-get install ffmpeg`)

**Лицензия:** FFmpeg распространяется под лицензией GPLv2/LGPLv2.1

**Ссылки:**
- Официальный сайт: https://ffmpeg.org/
- GitHub: https://github.com/FFmpeg/FFmpeg

---

## Структура проекта

```
onairchive/
├── Core/                          # Платформо-независимая бизнес-логика
├── Recorder/                      # Функционал записи
├── RecorderFileStorage/           # Управление файловым хранилищем
├── Logger/                        # Реализации логирования
├── ThirdParty/                    # Заголовочные файлы внешних библиотек
├── Platforms/                     # Платформо-специфичные точки входа
│   ├── Windows/
│   │   ├── GUI/                   # Windows GUI приложение
│   │   │   ├── OnAirchiveVCL.dpr
│   │   │   ├── OnAirchiveVCL.dproj
│   │   │   └── uMainForm.pas
│   │   └── SVC/                   # Windows Service
│   │       ├── OnAirchiveSVC.dpr
│   │       ├── OnAirchiveSVC.dproj
│   │       └── OnAirchiveSVCServiceUnit.pas
│   └── Linux/                     # Linux Console приложение
│       ├── OnAirchiveCLI.lpr
│       ├── OnAirchiveCLI.lpi
│       ├── build.sh               # Скрипт сборки
│       └── run-script.sh          # Скрипт запуска
├── Libs/                          # Бинарные библиотеки
│   ├── Windows/
│   │   └── x86/                   # 32-bit Windows
│   │       ├── bass.dll
│   │       ├── bass_aac.dll
│   │       ├── bassenc.dll
│   │       └── ffmpeg.exe
│   └── Linux/
│       └── x86_64/                # 64-bit Linux
│           ├── libbass.so
│           ├── libbass_aac.so
│           └── libbassenc.so
├── Config/                        # Пример конфигурационных файлов
│   └── config-example.ini
├── Output/                        # Результаты сборки (в .gitignore)
│   ├── Windows/
│   └── Linux/
├── Temp/                          # Временные файлы сборки (в .gitignore)
├── OnAirchive.groupproj           # Групповой проект Delphi
└── README.md                      # Этот файл
```

## Вклад в проект
Если вы нашли ошибку или у вас есть предложения по улучшению, не стесняйтесь:

- Создавать issue с описанием проблемы или предложения
- Предлагать pull request'ы с исправлениями или новыми функциями

## Лицензия

MIT