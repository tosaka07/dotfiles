local spaces = require "hs.spaces"
local timer = require "hs.timer"
local eventtap = require "hs.eventtap"
local events = eventtap.event.types
local module = {}

module.timeFrame = 0.5

-- Common functions

function ToggleApp(appName)
  local focusedSpace = spaces.focusedSpace()
  local app = hs.application.find(appName)

  if app == nil then
    hs.application.launchOrFocus(appName)
  elseif app:isFrontmost() then
    app:hide()
  else
    local window = app:focusedWindow()

    spaces.moveWindowToSpace(window, focusedSpace)

    hs.application.launchOrFocus(appName)
    hs.timer.doAfter(0.1, function()
      window:centerOnScreen()
    end)
  end
end

function GlobalHotKeys(appName, modifier, key)
  hs.hotkey.bind({ modifier }, key, function()
    ToggleApp(appName)
  end)
end

-- Settings

GlobalHotKeys("Wezterm", "ctrl", "return")


-- module.eventWatcher = eventtap.new({events.flagsChanged, events.keyDown}, function(ev)
--     if (timer.secondsSinceEpoch() - timeFirstControl) > module.timeFrame then
--         timeFirstControl, firstDown, secondDown = 0, false, false
--     end

--     if ev:getType() == events.flagsChanged then
--         if noFlags(ev) and firstDown and secondDown then
--             ToggleApp("Emacs")
--             timeFirstControl, firstDown, secondDown = 0, false, false
--         elseif onlyCtrl(ev) and not firstDown then
--             firstDown = true
--             timeFirstControl = timer.secondsSinceEpoch()
--         elseif onlyCtrl(ev) and firstDown then
--             secondDown = true
--         elseif not noFlags(ev) then
--             timeFirstControl, firstDown, secondDown = 0, false, false
--         end
--     else
--         timeFirstControl, firstDown, secondDown = 0, false, false
--     end
--     return false
-- end):start()
