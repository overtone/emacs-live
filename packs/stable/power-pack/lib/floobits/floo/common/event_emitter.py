class EventEmitter(object):
    def __init__(self):
        self._on_handlers = {}
        self._once_handlers = {}

    def on(self, event, handler):
        if event not in self._on_handlers:
            self._on_handlers[event] = []
        self._on_handlers[event].append(handler)

    def once(self, event, handler):
        if event not in self._once_handlers:
            self._once_handlers[event] = []
        self._once_handlers[event].append(handler)

    def emit(self, event, *args, **kwargs):
        handlers = self._once_handlers.pop(event, [])
        handlers += self._on_handlers.get(event, [])
        for handler in handlers:
            handler(*args, **kwargs)
