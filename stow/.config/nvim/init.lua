-- based on  nvim-lua / kickstart.nvim


-- todo:
-- other colorscheme(s)
-- other font(s)
-- window management/movement
-- terminal
-- keybindings
-- spelling correction

pwd = debug.getinfo(1).source:match("@?(.*/)")
if vim.g.vscode then
	dofile(pwd..'vscode.lua')
else
	dofile(pwd..'cli.lua')
end