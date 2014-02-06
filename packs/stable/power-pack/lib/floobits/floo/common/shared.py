import os

__VERSION__ = ''
__PLUGIN_VERSION__ = ''

# Config settings
USERNAME = ''
SECRET = ''
API_KEY = ''

DEBUG = False
SOCK_DEBUG = False

ALERT_ON_MSG = True
LOG_TO_CONSOLE = False

BASE_DIR = os.path.expanduser(os.path.join('~', 'floobits'))


# Shared globals
DEFAULT_HOST = 'floobits.com'
DEFAULT_PORT = 3448
SECURE = True


SHARE_DIR = None
COLAB_DIR = ''
PROJECT_PATH = ''
JOINED_WORKSPACE = False
PERMS = []
STALKER_MODE = False
SPLIT_MODE = False
MIRRORED_SAVES = True

AUTO_GENERATED_ACCOUNT = False
PLUGIN_PATH = None
WORKSPACE_WINDOW = None
CHAT_VIEW = None
CHAT_VIEW_PATH = None

TICK_TIME = 100
AGENT = None

IGNORE_MODIFIED_EVENTS = False
VIEW_TO_HASH = {}


FLOORC_PATH = os.path.expanduser(os.path.join('~', '.floorc'))
