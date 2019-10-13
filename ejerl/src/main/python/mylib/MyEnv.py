import gym


class MyEnv(object):
    def __init__(self):
        self.env = gym.make('CartPole-v0')

    def getStateDim(self):
        return self.env.observation_space.shape[0]

    def getActionCount(self):
        return self.env.action_space.n

    def reset(self):
        return self.env.reset()

    def step(self, action):
        # observation, reward, done, _
        return self.env.step(action)
