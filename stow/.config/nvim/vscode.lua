-- Install packer
local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
end

vim.api.nvim_exec(
  [[
  augroup Packer
    autocmd!
    autocmd BufWritePost init.lua PackerCompile
  augroup end
]],
  false
)

local use = require('packer').use
require('packer').startup(function()
  use 'wbthomason/packer.nvim' -- Package manager
  --use 'tpope/vim-commentary' -- "gc" to comment visual regions/lines
  use 'L3MON4D3/LuaSnip' -- Snippets plugin
  use 'folke/which-key.nvim'
end)

-- spell check
vim.spelllang='en,de'
vim.spellsuggest='best,9'

--Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = tru

--Remap space as leader key
vim.api.nvim_set_keymap('', '<Space>', '<Nop>', { noremap = true, silent = true })
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

----Remap for dealing with word wrap
--vim.api.nvim_set_keymap('n', 'k', "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
--vim.api.nvim_set_keymap('n', 'j', "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })

-- setup which key
vim.timeoutlen = 0
-- local wk = require("which-key")
-- wk.register({
--   f = {
--     name = "file", -- optional group name
--     f = { "<cmd>execute 'Telescope file_browser theme=ivy path='.expand('%:p:h')<cr>", "Find File" }, -- create a binding with label
--     -- b = { function() print("bar") end, "Foobar" } -- you can also pass functions!
--   },
--   b = {
--     name = "buffer", -- optional group name
--     b = { "<cmd>Telescope buffers<cr>", "Find buffer" },
--     r = { "<cmd>e!<cr>", "Reload current" },
--   },

--   w = {
-- 	name = "window",
-- 	v = {"<cmd>vsp<cr>", "Vertical Split"},
-- 	s = {"<cmd>sp<cr>", "Horizontal Split"},
-- 	h = {"<C-w>h", "left"},
-- 	j = {"<C-w>j", "down"},
-- 	k = {"<C-w>k", "up"},
-- 	l = {"<C-w>l", "right"},
-- 	mm = {"<cmd>only<cr>", "maximize"},
-- 	o = {"<cmd>only<cr>", "maximize"},
-- 	},
-- }, { prefix = "<leader>" })

-- make the config touchbar safe
for i=1,10 do
 	vim.api.nvim_set_keymap('', string.format("<F%d>", i), '<NOP>', { noremap = true, silent = true })
	vim.api.nvim_set_keymap('!', string.format("<F%d>", i), '<NOP>', { noremap = true, silent = true })
end
