{
  pkgs,
  ...
}:

{
  programs.nixvim = {
    enable = true;
    globals.mapleader = " ";
    
    opts = {
      tabstop = 2;
      softtabstop = 2;
      shiftwidth = 2;
      expandtab = true;
      smartindent = true;
    };

    plugins = {
      web-devicons.enable = true;
      
      telescope = {
        enable = true;
        keymaps = {
          "<leader>ff" = "find_files";
          "<leader>fg" = "live_grep";
        };
      };

      leap.enable = true;

      treesitter = {
        enable = true;
        grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
          bash
          json
          lua
          make
          markdown
          nix
          regex
          toml
          vim
          vimdoc
          xml
          yaml
        ];
      };

      lualine.enable = true;
      cmp-emoji.enable = true;

      # lsp
      cmp-nvim-lsp.enable = true;
      # current buffer
      cmp-buffer.enable = true;
      # fs paths
      cmp-path.enable = true;
      # snippets
      cmp_luasnip.enable = true;

      lsp = {
        enable = true;
        servers = {
          pylsp.enable = true;
          nixd.enable = true;
        };
      };

      cmp = {
        enable = true;
        settings = {
          autoEnableSources = true;
          experimental = {
            ghost_text = false;
          };
          performance = {
            debounce = 60;
            fetchingTimeout = 200;
            maxViewEntries = 30;
          };
          snippet = {
            expand = "luasnip";
          };
          formatting = {
            fields = [
              "kind"
              "abbr"
              "menu"
            ];
          };
          sources = [
            { name = "git"; }
            { name = "nvim_lsp"; }
            { name = "emoji"; }
            {
              name = "buffer"; # text within current buffer
              option.get_bufnrs.__raw = "vim.api.nvim_list_bufs";
              keywordLength = 3;
            }
            {
              name = "path"; # file system paths
              keywordLength = 3;
            }
            {
              name = "luasnip"; # snippets
              keywordLength = 3;
            }
          ];

          window = {
            completion = {
              border = "solid";
            };
            documentation = {
              border = "solid";
            };
          };

          mapping = {
            "<C-j>" = "cmp.mapping.select_next_item()";
            "<C-k>" = "cmp.mapping.select_prev_item()";
          };
        };
      };
    };
  };
}
