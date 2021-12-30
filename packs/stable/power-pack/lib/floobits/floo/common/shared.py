import os

__VERSION__ = ''
__PLUGIN_VERSION__ = ''

# Config settings
AUTH = {}

DEBUG = False
SOCK_DEBUG = False
SOCK_SINGLE_READ = False

EXPERT_MODE = False
FLOOTTY_SAFE = True

ALERT_ON_MSG = True
LOG_TO_CONSOLE = False
HEARTBEAT_TIMEOUT = 60

BASE_DIR = os.path.expanduser(os.path.join('~', 'floobits'))


# Shared globals
DEFAULT_HOST = 'floobits.com'
DEFAULT_PORT = 3448
SECURE = True
ERROR_COUNT = 0
ERRORS_SENT = 0
# Don't spam us with error reports
MAX_ERROR_REPORTS = 3

# For people who have outbound ports blocked (schools and BigCos)
OUTBOUND_FILTER_PROXY_HOST = 'proxy.floobits.com'
OUTBOUND_FILTER_PROXY_PORT = 443
OUTBOUND_FILTERING = False

PROXY_PORT = 0  # Random port
SHARE_DIR = None
COLAB_DIR = ''
PROJECT_PATH = ''
WORKSPACE_WINDOW = None

PERMS = []
FOLLOW_MODE = False
FOLLOW_USERS = set()
SPLIT_MODE = False

AUTO_GENERATED_ACCOUNT = False
PLUGIN_PATH = None

CHAT_VIEW = None
CHAT_VIEW_PATH = None

TICK_TIME = 100
AGENT = None
IGNORE = None

VIEW_TO_HASH = {}

FLOORC_JSON_PATH = os.path.expanduser(os.path.join('~', '.floorc.json'))

INSECURE_SSL = False  # Disable SSL cert valifation
