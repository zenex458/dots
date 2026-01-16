{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [../home/programs/firefox.nix];
  home.stateVersion = "25.11";
  home.file.".xinitrc" = {
    enable = true;
    text = ''
      xsetroot -cursor_name left_ptr &
      redshift -o -O 2000 &
      xrdb ~/.Xresources &
      exec xmonad
    '';
  };

  programs.tmux = {
    # add new-window -c "#{pane_current_path}"
    # add splitp -c "#{pane_current_path}"
    enable = true;
    aggressiveResize = true;
    prefix = "C-q";
    baseIndex = 0;
    escapeTime = 0;
    historyLimit = 100000;
    keyMode = "vi";
    mouse = true;
    terminal = "tmux-256color";
    extraConfig = ''
      set -g set-titles on
      set -g status-keys emacs
      set -s set-clipboard external
      set -g status-style "fg=#bdae93,bg=#060606"
      setw -g monitor-activity on
      set -g visual-activity on
      set -g status-right ""
      set -g status-left "#{session_group}"
      set -g window-status-current-format "#[fg=#060606 bg=#060606]|#[fg=#bdae93 bg=#060606]#W#[fg=#060606 bg=#060606]|"
      set -g window-status-last-style "fg=#a08a64 bg=#060606"
      bind-key -n M-"v" split-window -v
      bind-key -n M-"V" split-window -h
      bind-key -n M-h select-pane -L
      bind-key -n M-j select-pane -D
      bind-key -n M-k select-pane -U
      bind-key -n M-l select-pane -R
      bind-key -n M-H swap-pane -U
      bind-key -n M-J swap-pane -D
      bind-key -n M-K swap-pane -U
      bind-key -n M-L swap-pane -D
      bind-key -n M-C-h resize-pane -L
      bind-key -n M-C-j resize-pane -D
      bind-key -n M-C-k resize-pane -U
      bind-key -n M-C-l resize-pane -R
    '';
  };

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = pkgs.writeText "xmonad.hs" ''
      import XMonad
      main = xmonad defaultConfig
          { terminal    = "alacritty -e tmux"
          , modMask     = mod4Mask
          , borderWidth = 2
          }
    '';
  };
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: [
      epkgs.vterm
      epkgs.multi-vterm
    ];
    extraConfig = ''
       (setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/saves/")))
       (setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/saves/" t)))
       (load-theme 'modus-vivendi t)
    '';
  };
  programs.neovim = {
    enable = true;
    extraConfig = ''
      let mapleader = " "
      let g:currentmode={
             \ 'n'  : '[N] ',
             \ 'v'  : '[V] ',
             \ 'V'  : '[VLine] ',
             \ "\<C-V>" : '[VBlock] ',
             \ 'i'  : '[I] ',
             \ 'R'  : '[R] ',
             \ 'Rv' : '[V·Replace] ',
             \ 'c'  : '[Command] ',
             \}

      syntax on
      filetype plugin indent on
      set ruler
      set ignorecase
      set smartcase
      set smartindent
      set autoindent
      set cursorline
      set title
      set cursorcolumn
      set showcmd
      set showmatch
      set hlsearch
      set title
      set nocompatible
      set wildmode=longest,list,full
      set clipboard+=unnamedplus
      set termguicolors
      set noshowmode
      set guicursor=i:hor50-Cursor
      set guicursor+=i:blinkon100
      set guicursor+=n-v-c:blinkon100
      set statusline+=\%{toupper(g:currentmode[mode()])}
      set statusline+=%<%f%m\ \ \ %=\ %R%H%W\ %l/%L:%c\ %p%% "[%n] %Y
      "set guicursor=i:block-iCursor
      "set guicursor+=i:blinkon100
      "set guicursor+=n-v-c:blinkon100
      "set mouse=a
      "set linebreak
      set rnu nu
      colorscheme default
      inoremap <expr> <Tab> pumvisible() ? '<C-n>' :
      \ getline('.')[col('.')-2] =~# '[[:alnum:].-_#$]' ? '<C-x><C-o>' : '<Tab>'
      map <leader>r :%s/
      map <leader>+ <C-w>+
      map <leader>- <C-w>-
      map <leader>= <C-w>=
      map <leader>/ :noh<CR>
      map <leader>sp :setlocal spell! spelllang=en_gb<CR>
      tnoremap <Esc> <C-\><C-n>
      vmap <C-c> "+y
      map <leader>oe :bro ol<CR>
    '';
  };
  xresources = {
    properties = {
      "XTerm.vt100.foreground" = "#c6c6c6";
      "XTerm.vt100.background" = "#000000";
      "XTerm.vt100.cursorColor" = "#c6c6c6";
    };
  };

  programs.alacritty = {
    enable = true;
    settings = {
      env = {
        "TERM" = "xterm-256color";
      };

      #font = {
      #  size = 12.0;
      #  normal.family = "firacode";
      #  bold.family = "firacode";
      #  italic.family = "firacode";
      #};

      colors = {
        # Default colors
        primary = {
          background = "0x060606";
          foreground = "0xbdae93";
        };

        # Normal colors
        normal = {
          black = "0x100e23";
          red = "0xff8080";
          green = "0x95ffa4";
          yellow = "0xffe9aa";
          blue = "0x91ddff";
          magenta = "0xc991e1";
          cyan = "0xaaffe4";
          white = "0xcbe3e7";
        };

        # Bright colors
        bright = {
          black = "0x565575";
          red = "0xff5458";
          green = "0x62d196";
          yellow = "0xffb378";
          blue = "0x65b2ff";
          magenta = "0x906cff";
          cyan = "0x63f2f1";
          white = "0xa6b3cc";
        };
      };
    };
  };
}
