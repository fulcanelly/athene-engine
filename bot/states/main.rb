class MainMenuState < BaseState 

    def run 

        if myself.channels.empty? then 
            say "You need to add at least one channel to use bot"
            switch_state AddChannelState.new
        else 
            suggest_it("What to do ?")
                .option("Add channel") do
                    switch_state AddChannelState.new
                end
                .option("My channels") do end
                .option("None") do end
                .exec
        end

        # end

        switch_state MainMenuState.new 
    end

end
