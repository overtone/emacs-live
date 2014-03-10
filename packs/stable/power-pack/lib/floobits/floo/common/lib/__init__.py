try:
    from . import dmp_monkey
except ImportError:
    import dmp_monkey

dmp_monkey.monkey_patch()

try:
    from . import diff_match_patch
except ImportError:
    import diff_match_patch

DMP = diff_match_patch.diff_match_patch()
