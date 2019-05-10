using Ninject.Modules;
using Ninject;
using MultiAgentLanguageModels;

public class Bindings : NinjectModule
{
    public override void Load()
    {
        Bind<IPrologService>().To<PrologService>();
    }
}