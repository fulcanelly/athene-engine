class MainMenuState < BaseState 

    def run 

        # if myself.channel.empty? then 
        say "You need add channel"
        switch_state AddChannelState.new
        
        # end

        switch_state MainMenuState.new 
    end

end
