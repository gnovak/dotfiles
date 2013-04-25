# Configuration file for ipython.

c = get_config()
c.TerminalInteractiveShell.colors = 'LightBG'
c.InteractiveShellApp.pylab = 'osx'
c.InteractiveShellApp.extensions = ['autoreload', 'grasp']
c.InteractiveShellApp.exec_lines = ['%autoreload 2']


