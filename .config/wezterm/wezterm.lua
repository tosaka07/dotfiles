local wezterm = require 'wezterm'
local act = wezterm.action

function merge_table(table1, table2)
  for _, value in ipairs(table2) do
    table1[#table1+1] = value
  end
  return table1
end

wezterm.on("update-right-status", function(window, pane)
  local leader = "NORMAL"
  if window:leader_is_active() then
    leader = "LEADER"
  end
  window:set_right_status(leader.."  ")
end);

local tabkeys = {}
for i = 1, 9 do
  table.insert(tabkeys, {
    key=tostring(i),
    mods="LEADER",
    action=act.ActivateTab(i - 1),
  })
end

local commonkeys = {
    { key="c", mods="LEADER", action=act.SpawnTab("CurrentPaneDomain") },
    { key="x", mods="LEADER|SHIFT", action=act.CloseCurrentTab{confirm=true} },
    { key="x", mods="LEADER", action=act.CloseCurrentPane{confirm=true} },
    { key="|", mods="LEADER|SHIFT",action=act.SplitHorizontal{domain="CurrentPaneDomain"} },
    { key="-", mods="LEADER", action=act.SplitVertical{domain="CurrentPaneDomain"} },
    { key = "l", mods = "LEADER", action = act {ActivatePaneDirection = "Right"} },
    { key = "h", mods = "LEADER", action = act {ActivatePaneDirection = "Left"} },
    { key = "k", mods = "LEADER", action = act {ActivatePaneDirection = "Up"} },
    { key = "j", mods = "LEADER", action = act {ActivatePaneDirection = "Down"} },
    { key = "l", mods = "LEADER|SHIFT", action = act.ActivateTabRelative(1) },
    { key = "h", mods = "LEADER|SHIFT", action = act.ActivateTabRelative(-1) },
}

return {
  use_ime = true,
  font_size = 14,
  font = wezterm.font_with_fallback {
    -- "UDEV Gothic 35",
    "UDEV Gothic 35NFLG",
    -- "all-the-icons",
    -- "github-octicons",
    -- "file-icons",
    -- "Weather Icons",
    "icons-in-terminal",
  },
  color_scheme = "nord",
  window_background_opacity = 0.95,
  text_background_opacity = 1.0,
  window_decorations = "RESIZE",
  skip_close_confirmation_for_processes_named = {"tmux"},
  leader = { key="Space", mods="CTRL" },
  keys = merge_table(tabkeys, commonkeys),
  key_tables = {
    copy_mode = {
      {key="c", mods="CTRL", action=act.CopyMode("Close")},
      {key="g", mods="CTRL", action=act.CopyMode("Close")},
      {key="q", mods="NONE", action=act.CopyMode("Close")},
      {key="Escape", mods="NONE", action=act.CopyMode("Close")},

      {key="h", mods="NONE", action=act.CopyMode("MoveLeft")},
      {key="j", mods="NONE", action=act.CopyMode("MoveDown")},
      {key="k", mods="NONE", action=act.CopyMode("MoveUp")},
      {key="l", mods="NONE", action=act.CopyMode("MoveRight")},

      {key="LeftArrow",  mods="NONE", action=act.CopyMode("MoveLeft")},
      {key="DownArrow",  mods="NONE", action=act.CopyMode("MoveDown")},
      {key="UpArrow",    mods="NONE", action=act.CopyMode("MoveUp")},
      {key="RightArrow", mods="NONE", action=act.CopyMode("MoveRight")},

      {key="RightArrow", mods="ALT",  action=act.CopyMode("MoveForwardWord")},
      {key="f",          mods="ALT",  action=act.CopyMode("MoveForwardWord")},
      {key="Tab",        mods="NONE", action=act.CopyMode("MoveForwardWord")},
      {key="w",          mods="NONE", action=act.CopyMode("MoveForwardWord")},
      {key="e",          mods="NONE", action=act.CopyMode("MoveForwardWord")},

      {key="LeftArrow", mods="ALT",   action=act.CopyMode("MoveBackwardWord")},
      {key="b",         mods="ALT",   action=act.CopyMode("MoveBackwardWord")},
      {key="Tab",       mods="SHIFT", action=act.CopyMode("MoveBackwardWord")},
      {key="b",         mods="NONE",  action=act.CopyMode("MoveBackwardWord")},

      {key="0",     mods="NONE",  action=act.CopyMode("MoveToStartOfLine")},
      {key="Enter", mods="NONE",  action=act.CopyMode("MoveToStartOfNextLine")},

      {key="$",     mods="NONE",  action=act.CopyMode("MoveToEndOfLineContent")},
      {key="$",     mods="SHIFT", action=act.CopyMode("MoveToEndOfLineContent")},
      {key="^",     mods="NONE",  action=act.CopyMode("MoveToStartOfLineContent")},
      {key="^",     mods="SHIFT", action=act.CopyMode("MoveToStartOfLineContent")},
      {key="m",     mods="ALT",   action=act.CopyMode("MoveToStartOfLineContent")},

      {key=" ", mods="NONE",  action=act.CopyMode{SetSelectionMode="Cell"}},
      {key="v", mods="NONE",  action=act.CopyMode{SetSelectionMode="Cell"}},
      {key="V", mods="NONE",  action=act.CopyMode{SetSelectionMode="Line"}},
      {key="V", mods="SHIFT", action=act.CopyMode{SetSelectionMode="Line"}},
      {key="v", mods="CTRL",  action=act.CopyMode{SetSelectionMode="Block"}},

      {key="G", mods="NONE",  action=act.CopyMode("MoveToScrollbackBottom")},
      {key="G", mods="SHIFT", action=act.CopyMode("MoveToScrollbackBottom")},
      {key="g", mods="NONE",  action=act.CopyMode("MoveToScrollbackTop")},

      {key="H", mods="NONE",  action=act.CopyMode("MoveToViewportTop")},
      {key="H", mods="SHIFT", action=act.CopyMode("MoveToViewportTop")},
      {key="M", mods="NONE",  action=act.CopyMode("MoveToViewportMiddle")},
      {key="M", mods="SHIFT", action=act.CopyMode("MoveToViewportMiddle")},
      {key="L", mods="NONE",  action=act.CopyMode("MoveToViewportBottom")},
      {key="L", mods="SHIFT", action=act.CopyMode("MoveToViewportBottom")},

      {key="o", mods="NONE",  action=act.CopyMode("MoveToSelectionOtherEnd")},
      {key="O", mods="NONE",  action=act.CopyMode("MoveToSelectionOtherEndHoriz")},
      {key="O", mods="SHIFT", action=act.CopyMode("MoveToSelectionOtherEndHoriz")},

      {key="PageUp",   mods="NONE", action=act.CopyMode("PageUp")},
      {key="PageDown", mods="NONE", action=act.CopyMode("PageDown")},

      {key="b", mods="CTRL", action=act.CopyMode("PageUp")},
      {key="f", mods="CTRL", action=act.CopyMode("PageDown")},
    }
  },
}
