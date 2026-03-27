-- Install packer
local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
end

vim.api.nvim_create_augroup('Packer', { clear = true })
vim.api.nvim_create_autocmd('BufWritePost', {
  group = 'Packer',
  pattern = 'init.lua',
  command = 'PackerCompile',
})

local use = require('packer').use
require('packer').startup(function()
  use 'wbthomason/packer.nvim' -- Package manager
  use 'tpope/vim-fugitive' -- Git commands in nvim
  use 'tpope/vim-commentary' -- "gc" to comment visual regions/lines
  use { 'nvim-telescope/telescope.nvim', requires = { 'nvim-lua/plenary.nvim' } }
  use { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }
  use { 'nvim-telescope/telescope-file-browser.nvim' }
  use { 'catppuccin/nvim', as = 'catppuccin' }
  use 'itchyny/lightline.vim'
  use 'lukas-reineke/indent-blankline.nvim'
  use { 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' } }
  use 'neovim/nvim-lspconfig'
  use 'folke/which-key.nvim'
end)

vim.o.clipboard = 'unnamed,unnamedplus'

vim.o.termguicolors = true
-- flavour: latte (light) | frappe (dark) | macchiato | mocha
require('catppuccin').setup({ flavour = 'latte' })
local ok, _ = pcall(vim.cmd, 'colorscheme catppuccin')
if not ok then
  vim.notify("colorscheme 'catppuccin' not found — run :PackerSync", vim.log.levels.WARN)
end
vim.o.inccommand = 'nosplit'
vim.o.hlsearch = false
vim.wo.number = true
vim.o.hidden = true
vim.o.mouse = 'a'
vim.o.breakindent = true
vim.opt.undofile = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'

local ivy = { disable_devicons = true, theme = 'ivy' }
local telescope = require('telescope')
telescope.load_extension('fzf')
telescope.load_extension('file_browser')
telescope.setup({
  pickers = {
    find_files = ivy,
    buffers = ivy,
    oldfiles = ivy,
    grep_string = ivy,
    live_grep = ivy,
  },
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = "smart_case",
    },
    file_browser = {
      theme = "ivy",
    },
  },
})

vim.g.lightline = {
  colorscheme = 'catppuccin',
  active = { left = { { 'mode', 'paste' }, { 'gitbranch', 'readonly', 'filename', 'modified' } } },
  component_function = { gitbranch = 'fugitive#head' },
}

-- Space as leader key
vim.keymap.set('', '<Space>', '<Nop>', { silent = true })
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Navigate visual lines when no count given
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Terminal
vim.api.nvim_create_augroup('custom_term', { clear = true })
vim.api.nvim_create_autocmd('TermOpen', {
  group = 'custom_term',
  callback = function()
    vim.wo.number = false
    vim.wo.relativenumber = false
    vim.bo.bufhidden = 'hide'
  end,
})
vim.keymap.set('t', '<Esc>', '<C-\\><C-n>')

-- Highlight on yank
vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  group = 'YankHighlight',
  callback = function() vim.highlight.on_yank() end,
})

vim.keymap.set('n', 'Y', 'y$')

-- indent-blankline
vim.g.indent_blankline_char = '┊'
vim.g.indent_blankline_filetype_exclude = { 'help', 'packer' }
vim.g.indent_blankline_buftype_exclude = { 'terminal', 'nofile' }
vim.g.indent_blankline_char_highlight = 'LineNr'
vim.g.indent_blankline_show_trailing_blankline_indent = false

require('gitsigns').setup {
  signs = {
    add = { hl = 'GitGutterAdd', text = '+' },
    change = { hl = 'GitGutterChange', text = '~' },
    delete = { hl = 'GitGutterDelete', text = '_' },
    topdelete = { hl = 'GitGutterDelete', text = '‾' },
    changedelete = { hl = 'GitGutterChange', text = '~' },
  },
}

-- LSP keymaps (nvim 0.11+)
vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(args)
    local opts = { buffer = args.buf, silent = true }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
    vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, opts)
    vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, opts)
    vim.keymap.set('n', '<leader>D', vim.lsp.buf.type_definition, opts)
    vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, opts)
    vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, opts)
    vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
    vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
    vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, opts)
    vim.keymap.set('n', '<leader>so', function() require('telescope.builtin').lsp_document_symbols() end, opts)
  end,
})

vim.lsp.config('pyright', {})
vim.lsp.config('texlab', {})
vim.lsp.enable({ 'pyright', 'texlab' })

-- which-key
vim.o.timeoutlen = 0
local wk = require("which-key")
wk.add({
  { "<leader>f", group = "file" },
  { "<leader>ff", "<cmd>execute 'Telescope file_browser theme=ivy path='.expand('%:p:h')<cr>", desc = "Find File" },
  { "<leader>b", group = "buffer" },
  { "<leader>bb", "<cmd>Telescope buffers<cr>", desc = "Find buffer" },
  { "<leader>br", "<cmd>e!<cr>", desc = "Reload current" },
  { "<leader>w", group = "window" },
  { "<leader>wv", "<cmd>vsp<cr>", desc = "Vertical Split" },
  { "<leader>ws", "<cmd>sp<cr>", desc = "Horizontal Split" },
  { "<leader>wh", "<C-w>h", desc = "left" },
  { "<leader>wj", "<C-w>j", desc = "down" },
  { "<leader>wk", "<C-w>k", desc = "up" },
  { "<leader>wl", "<C-w>l", desc = "right" },
  { "<leader>wo", "<cmd>only<cr>", desc = "maximize" },
  { "<leader>p", '"+p', desc = "[p]aste from clipboard" },
  { "<leader>P", '"+P', desc = "[P]aste from clipboard" },
  { "<leader>y", '"+y', desc = "[y]ank to clipboard" },
  { "<leader>Y", '"+yy', desc = "[yy]ank to clipboard" },
})

-- disable function keys (touchbar safety)
for i = 1, 10 do
  vim.keymap.set({ '', '!' }, string.format("<F%d>", i), '<NOP>')
end
