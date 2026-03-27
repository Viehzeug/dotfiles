-- based on  nvim-lua / kickstart.nvim


pwd = debug.getinfo(1).source:match("@?(.*/)")
if vim.g.vscode then
	dofile(pwd..'vscode.lua')
else
	dofile(pwd..'cli.lua')
end
