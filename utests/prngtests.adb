--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNGtests_Suite;

with AUnit.Run;
with AUnit.Reporter.Text;

procedure PRNGtests is
   procedure Run is new AUnit.Run.Test_Runner (PRNGtests_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end PRNGtests;
